#
#
#           nph
#        (c) Copyright 2023 Jacek Sieka
#           The Nim compiler
#        (c) Copyright 2018 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
# This module implements the renderer of the standard Nim representation.
# nph version:
# * generate code that actually can be parsed by the parser
# * more lookahead formatting
# * drive comment layout from here instead of lexer
# 'import renderer' is so useful for debugging
# that Nim shouldn't produce a warning for that:

{.used.}

import "$nim"/compiler/[idents], std/[strutils, sequtils]
import "."/[phlexer, phoptions, phast, phmsgs, phlineinfos]
when defined(nimPreviewSlimSystem):
  import std/[syncio, assertions, formatfloat]

const
  IndentWidth = 2
  longIndentWid = IndentWidth * 2
  MaxLineLen = 88
  LineCommentColumn = 30
  blankAfterComplex = {nkObjectTy, nkEnumTy, nkTypeSection, nkProcDef .. nkIteratorDef}

type
  TRenderTok* = object
    kind*: TokType
    length*: int16
    sym*: PSym

  TRenderTokSeq* = seq[TRenderTok]
  TSrcGen* = object
    indent*: int
    lineLen*: int
    line: int # The line where a token was last output
    col: int
    pos*: int # current position for iteration over the buffer
    idx*: int # current token index for iteration over the buffer
    tokens*: TRenderTokSeq
    buf*: string
    pendingNL*: int
      # negative if not active; else contains the
      # indentation value

    pendingWhitespace: int
    comStack*: seq[PNode] # comment stack
    checkAnon: bool # we're in a context that can contain sfAnon
    inPragma: int
    inImportLike: int
    pendingNewline: bool
    fid*: FileIndex
    config*: ConfigRef
    mangler: seq[PSym]

  ListFlag = enum
    lfFirstSticky
      ## Render the first item of a list on the same line even if it doesn't fit
    lfSepAtEnd ## Always add separator at end
    lfLongSepAtEnd ## Add separator at end in one-per-line mode
    lfSkipPushComma ## Hack to remove first comma in pragma push

  ListFlags = set[ListFlag]
  SubFlag = enum
    sfNoIndent ## Already performed the indent for this sub (usually an infix)
    sfLongIndent ## We're about to hit an indented section so make a double-indent
    sfSkipPostfix ## the caller will handle rendering the postfix

  SubFlags = set[SubFlag]

proc renderTree*(n: PNode; conf: ConfigRef = nil): string

proc isSimple(n: PNode): bool =
  ## Simple nodes are those that are either identifiers or simple lists thereof
  case n.kind
  of nkCharLit .. nkNilLit, nkIdent:
    true
  of nkStmtList, nkImportStmt, nkExportStmt:
    n.allIt(isSimple(it))
  else:
    false

proc gsubAddsPar(n: PNode): bool =
  # There's an ambiguity in the grammar where nkPar sometimes is turned into
  # nkStmtListExpr - this helper is a stopgap solution to work around
  # the parsing quirk but needs more thought put into it
  n.kind == nkStmtListExpr and n.len == 1 and
    n[0].kind in {nkDiscardStmt, nkIfStmt, nkBlockStmt}

# We render the source code in a two phases: The first
# determines how long the subtree will likely be, the second
# phase appends to a buffer that will be the output.
proc isKeyword*(i: PIdent): bool =
  if (i.id >= ord(tokKeywordLow) - ord(tkSymbol)) and
      (i.id <= ord(tokKeywordHigh) - ord(tkSymbol)):
    result = true

proc isExported(n: PNode): bool =
  ## Checks if an ident is exported.
  ## This is meant to be used with idents in nkIdentDefs.
  case n.kind
  of nkPostfix:
    n[0].ident.s == "*" and n[1].kind == nkIdent
  of nkPragmaExpr:
    n[0].isExported()
  else:
    false

proc renderDefinitionName*(s: PSym; noQuotes = false): string =
  ## Returns the definition name of the symbol.
  ##
  ## If noQuotes is false the symbol may be returned in backticks. This will
  ## happen if the name happens to be a keyword or the first character is not
  ## part of the SymStartChars set.
  let x = s.name.s
  if noQuotes or (x[0] in SymStartChars and not phrenderer.isKeyword(s.name)):
    result = x
  else:
    result = '`' & x & '`'

proc initSrcGen(g: var TSrcGen; config: ConfigRef) =
  g.comStack = @[]
  g.tokens = @[]
  g.indent = 0
  g.lineLen = 0
  g.pos = 0
  g.idx = 0
  g.buf = ""
  g.pendingNL = -1
  g.pendingWhitespace = -1
  g.config = config

proc addTok(g: var TSrcGen; kind: TokType; s: string; sym: PSym = nil) =
  # debugEcho "addTok ", kind, " " , s.len, " ", s
  g.tokens.add TRenderTok(kind: kind, length: int16(s.len), sym: sym)

  g.buf.add(s)
  if kind != tkSpaces:
    inc g.col, s.len

  g.line += count(s, "\n")

proc outputLine(g: TSrcGen): int =
  ## The line it would be added if we were to add a token
  g.line +
    (if g.pendingNL >= 0: 1 + ord(g.pendingNewline)
    else: 0
    )

proc addPendingNL(g: var TSrcGen) =
  if g.pendingNL >= 0:
    let newlines = repeat("\n", 1 + ord(g.pendingNewline))
    g.pendingNewline = false

    addTok(g, tkSpaces, newlines & spaces(g.pendingNL))

    g.lineLen = g.pendingNL
    g.col = g.pendingNL
    g.pendingNL = -1
    g.pendingWhitespace = -1
  elif g.pendingWhitespace >= 0:
    addTok(g, tkSpaces, spaces(g.pendingWhitespace))

    g.pendingWhitespace = -1

proc optNL(g: var TSrcGen; indent: int) =
  g.pendingNL = indent
  g.lineLen = indent
  g.col = g.indent

proc optNL(g: var TSrcGen) =
  optNL(g, g.indent)

proc optNL(g: var TSrcGen; a, b: PNode) =
  g.pendingNL = g.indent
  g.lineLen = g.indent
  g.col = g.indent

  g.pendingNewline = g.pendingNewline or a.endInfo.line + 1 < b.info.line

proc blankLine(g: var TSrcGen; v = true) =
  g.pendingNewline = v or g.pendingNewline

proc indentNL(g: var TSrcGen; wid = IndentWidth) =
  inc(g.indent, wid)

  g.pendingNL = g.indent
  g.lineLen = g.indent

proc dedent(g: var TSrcGen; wid = IndentWidth) =
  dec(g.indent, wid)
  assert(g.indent >= 0)
  if g.pendingNL > wid:
    dec(g.pendingNL, wid)
    dec(g.lineLen, wid)

template withIndent(g: var TSrcGen; indentParam: int; body: untyped) =
  g.indent += indentParam

  body

  g.indent -= indentParam

template withIndent(g: var TSrcGen; body: untyped) =
  withIndent(g, IndentWidth, body)

proc put(g: var TSrcGen; kind: TokType; s: string; sym: PSym = nil) =
  # debugEcho "put ", kind, " ", s
  if kind != tkSpaces:
    addPendingNL(g)
    if s.len > 0 or kind in {tkHideableStart, tkHideableEnd}:
      addTok(g, kind, s, sym)
  else:
    g.pendingWhitespace = s.len

    inc g.col, s.len

  inc(g.lineLen, s.len)

proc putComment(g: var TSrcGen; s: string) =
  if s.len == 0:
    return

  if g.lineLen > 0 and g.pendingNL < 0 and g.pendingWhitespace < 0:
    put(g, tkSpaces, " ")

  put(g, tkComment, s)

  # TODO no implied eol on multiline comments
  optNL(g)

proc containsNL(s: string): bool =
  for i in 0 ..< s.len:
    case s[i]
    of '\r', '\n':
      return true
    else:
      discard

  result = false

const Space = " "

proc lsub(g: TSrcGen; n: PNode): int

proc litAux(g: TSrcGen; n: PNode; x: BiggestInt; size: int): string =
  proc skip(t: PType): PType =
    result = t
    while result != nil and
        result.kind in
        {tyGenericInst, tyRange, tyVar, tyLent, tyDistinct, tyOrdinal, tyAlias, tySink}:
      result = lastSon(result)

  let typ = n.typ.skip
  if typ != nil and typ.kind in {tyBool, tyEnum}:
    if sfPure in typ.sym.flags:
      result = typ.sym.name.s & '.'

    let enumfields = typ.n
    # we need a slow linear search because of enums with holes:
    for e in items(enumfields):
      if e.sym.position == x:
        result &= e.sym.name.s

        return

  if nfBase2 in n.flags:
    result = "0b" & toBin(x, size * 8)
  elif nfBase8 in n.flags:
    var y =
      if size < sizeof(BiggestInt):
        x and ((1.BiggestInt shl (size * 8)) - 1)
      else:
        x

    result = "0o" & toOct(y, size * 3)
  elif nfBase16 in n.flags:
    result = "0x" & toHex(x, size * 2)
  else:
    result = $x

proc ulitAux(g: TSrcGen; n: PNode; x: BiggestInt; size: int): string =
  if nfBase2 in n.flags:
    result = "0b" & toBin(x, size * 8)
  elif nfBase8 in n.flags:
    result = "0o" & toOct(x, size * 3)
  elif nfBase16 in n.flags:
    result = "0x" & toHex(x, size * 2)
  else:
    result = $cast[BiggestUInt](x)

proc atom(g: TSrcGen; n: PNode): string =
  doAssert g.config != nil, "g.config not initialized!"
  if n.info.offsetA <= n.info.offsetB:
    # for some constructed tokens this can not be the case and we're better
    # off to not mess with the offset then.
    return fileSection(g.config, g.fid, n.info.offsetA, n.info.offsetB)

  var f: float32
  case n.kind
  of nkEmpty:
    result = ""
  of nkIdent:
    result = n.ident.s
  of nkSym:
    result = n.sym.name.s
  of nkClosedSymChoice, nkOpenSymChoice:
    result = n[0].sym.name.s
  of nkStrLit:
    result = ""

    result.addQuoted(n.strVal)
  of nkRStrLit:
    result = "r\"" & replace(n.strVal, "\"", "\"\"") & '\"'
  of nkTripleStrLit:
    result = "\"\"\"" & n.strVal & "\"\"\""
  of nkCharLit:
    result = "\'"

    result.addEscapedChar(chr(int(n.intVal)))

    result.add '\''
  of nkIntLit:
    result = litAux(g, n, n.intVal, 4)
  of nkInt8Lit:
    result = litAux(g, n, n.intVal, 1) & "\'i8"
  of nkInt16Lit:
    result = litAux(g, n, n.intVal, 2) & "\'i16"
  of nkInt32Lit:
    result = litAux(g, n, n.intVal, 4) & "\'i32"
  of nkInt64Lit:
    result = litAux(g, n, n.intVal, 8) & "\'i64"
  of nkUIntLit:
    result = ulitAux(g, n, n.intVal, 4) & "\'u"
  of nkUInt8Lit:
    result = ulitAux(g, n, n.intVal, 1) & "\'u8"
  of nkUInt16Lit:
    result = ulitAux(g, n, n.intVal, 2) & "\'u16"
  of nkUInt32Lit:
    result = ulitAux(g, n, n.intVal, 4) & "\'u32"
  of nkUInt64Lit:
    result = ulitAux(g, n, n.intVal, 8) & "\'u64"
  of nkFloatLit:
    if n.flags * {nfBase2, nfBase8, nfBase16} == {}:
      result = $(n.floatVal)
    else:
      result = litAux(g, n, (cast[ptr int64](addr(n.floatVal)))[], 8)
  of nkFloat32Lit:
    if n.flags * {nfBase2, nfBase8, nfBase16} == {}:
      result = $n.floatVal & "\'f32"
    else:
      f = n.floatVal.float32
      result = litAux(g, n, (cast[ptr int32](addr(f)))[], 4) & "\'f32"
  of nkFloat64Lit:
    if n.flags * {nfBase2, nfBase8, nfBase16} == {}:
      result = $n.floatVal & "\'f64"
    else:
      result = litAux(g, n, (cast[ptr int64](addr(n.floatVal)))[], 8) & "\'f64"
  of nkNilLit:
    result = "nil"
  of nkType:
    if (n.typ != nil) and (n.typ.sym != nil):
      result = n.typ.sym.name.s
    else:
      result = "[type node]"
  else:
    internalError(g.config, "renderer.atom " & $n.kind)

    result = ""

proc lcomma(g: TSrcGen; n: PNode; start: int = 0; theEnd: int = -1): int =
  assert(theEnd < 0)
  # if n.prefix.len > 0 or n.mid.len > 0 or n.postfix.len > 0:
  #   return MaxLineLen + 1
  result = 0
  for i in start .. n.len + theEnd:
    let param = n[i]
    if nfDefaultParam notin param.flags:
      inc(result, lsub(g, param))
      inc(result, 2) # for ``, ``

  if result > 0:
    dec(result, 2) # last does not get a comma!

proc lsons(g: TSrcGen; n: PNode; start: int = 0; theEnd: int = -1): int =
  assert(theEnd < 0)

  result = 0
  # if n.prefix.len > 0 or n.mid.len > 0 or n.postfix.len > 0:
  #   return MaxLineLen + 1
  for i in start .. n.len + theEnd:
    inc(result, lsub(g, n[i]))

proc origUsingType(n: PNode): PSym {.inline.} =
  ## Returns the type that a parameter references. Check with referencesUsing first
  ## to check `n` is actually referencing a using node
  # If the node is untyped the typ field will be nil
  if n[0].sym.typ != nil:
    n[0].sym.typ.sym
  else:
    nil

proc referencesUsing(n: PNode): bool =
  ## Returns true if n references a using statement.
  ## e.g. proc foo(x) # x doesn't have type or def value so it references a using
  result =
    n.kind == nkIdentDefs and
      # Sometimes the node might not have been semmed (e.g. doc0) and will be nkIdent instead
    n[0].kind == nkSym and
      # Templates/macros can have parameters with no type (But their orig type will be nil)
    n.origUsingType != nil and n[1].kind == nkEmpty and n[2].kind == nkEmpty

proc lsub(g: TSrcGen; n: PNode): int =
  # computes the length of a tree
  if isNil(n):
    return 0
  # if n.prefix.len > 0 or n.mid.len > 0 or n.postfix.len > 0:
  #   return MaxLineLen + 1
  # if shouldRenderComment(g, n):
  #   return MaxLineLen + 1

  case n.kind
  of nkEmpty:
    result = 0
  of nkTripleStrLit:
    if containsNL(n.strVal):
      result = MaxLineLen + 1
    else:
      result = atom(g, n).len
  of succ(nkEmpty) .. pred(nkTripleStrLit), succ(nkTripleStrLit) .. nkNilLit:
    result = atom(g, n).len
  of nkCall, nkBracketExpr, nkCurlyExpr, nkConv, nkPattern, nkObjConstr:
    result = lsub(g, n[0]) + lcomma(g, n, 1) + len("()")
  of nkHiddenStdConv, nkHiddenSubConv, nkHiddenCallConv:
    result = lsub(g, n[1])
  of nkCast:
    result = lsub(g, n[0]) + lsub(g, n[1]) + len("cast[]()")
  of nkAddr:
    result =
      (if n.len > 0:
        lsub(g, n[0]) + len("addr()")
      else: 4
      )
  of nkStaticExpr:
    result = lsub(g, n[0]) + len("static_")
  of nkHiddenAddr, nkHiddenDeref, nkStringToCString, nkCStringToString:
    result = lsub(g, n[0])
  of nkCommand:
    result = lsub(g, n[0]) + lcomma(g, n, 1) + 1
  of nkExprEqExpr, nkAsgn, nkFastAsgn:
    result = lsons(g, n) + 3
  of nkPar, nkCurly, nkBracket, nkClosure:
    result = lcomma(g, n) + 2
  of nkTupleConstr:
    # assume the trailing comma:
    result = lcomma(g, n) + 3
  of nkArgList:
    result = lcomma(g, n)
  of nkTableConstr:
    result =
      if n.len > 0:
        lcomma(g, n) + 2
      else:
        len("{:}")
  of nkClosedSymChoice, nkOpenSymChoice:
    if n.len > 0:
      result += lsub(g, n[0])
  of nkTupleTy:
    result = lcomma(g, n) + len("tuple[]")
  of nkTupleClassTy:
    result = len("tuple")
  of nkDotExpr:
    result = lsons(g, n) + 1
  of nkBind:
    result = lsons(g, n) + len("bind_")
  of nkBindStmt:
    result = lcomma(g, n) + len("bind_")
  of nkMixinStmt:
    result = lcomma(g, n) + len("mixin_")
  of nkCheckedFieldExpr:
    result = lsub(g, n[0])
  of nkLambda:
    result = lsons(g, n) + len("proc__=_")
  of nkDo:
    result = lsons(g, n) + len("do__:_")
  of nkConstDef, nkIdentDefs:
    result = lcomma(g, n, 0, -3)
    if n.referencesUsing:
      result += lsub(g, newSymNode(n.origUsingType)) + 2
    else:
      if n[^2].kind != nkEmpty:
        result += lsub(g, n[^2]) + 2

      if n[^1].kind != nkEmpty:
        result += lsub(g, n[^1]) + 3
  of nkVarTuple:
    if n[^1].kind == nkEmpty:
      result = lcomma(g, n, 0, -2) + len("()")
    else:
      result = lcomma(g, n, 0, -3) + len("() = ") + lsub(g, lastSon(n))
  of nkChckRangeF:
    result = len("chckRangeF") + 2 + lcomma(g, n)
  of nkChckRange64:
    result = len("chckRange64") + 2 + lcomma(g, n)
  of nkChckRange:
    result = len("chckRange") + 2 + lcomma(g, n)
  of nkObjDownConv, nkObjUpConv:
    result = 2
    if n.len >= 1:
      result += lsub(g, n[0])

    result += lcomma(g, n, 1)
  of nkExprColonExpr:
    result = lsons(g, n) + 2
  of nkInfix:
    result = lsons(g, n) + 2
  of nkPrefix:
    result =
      lsons(g, n) + 1 +
        (if n.len > 0 and n[1].kind == nkInfix:
          2
        else: 0
        )
  of nkPostfix:
    result = lsons(g, n)
  of nkCallStrLit:
    result = lsons(g, n)
  of nkPragmaExpr:
    result = lsub(g, n[0]) + lcomma(g, n, 1)
  of nkRange:
    result = lsons(g, n) + 2
  of nkDerefExpr:
    result = lsub(g, n[0]) + 2
  of nkAccQuoted:
    result = lsons(g, n) + 2
  of nkIfExpr:
    result = lsub(g, n[0][0]) + lsub(g, n[0][1]) + lsons(g, n, 1) + len("if_:_")
  of nkElifExpr:
    result = lsons(g, n) + len("_elif_:_")
  of nkElseExpr:
    result = lsub(g, n[0]) + len("_else:_") # type descriptions
  of nkTypeOfExpr:
    result =
      (if n.len > 0:
        lsub(g, n[0])
      else: 0
      ) + len("typeof()")
  of nkRefTy:
    result =
      (if n.len > 0:
        lsub(g, n[0]) + 1
      else: 0
      ) + len("ref")
  of nkPtrTy:
    result =
      (if n.len > 0:
        lsub(g, n[0]) + 1
      else: 0
      ) + len("ptr")
  of nkVarTy, nkOutTy:
    result =
      (if n.len > 0:
        lsub(g, n[0]) + 1
      else: 0
      ) + len("var")
  of nkDistinctTy:
    result =
      len("distinct") +
        (if n.len > 0:
          lsub(g, n[0]) + 1
        else: 0
        )

    if n.len > 1:
      result +=
        (if n[1].kind == nkWith:
          len("_with_")
        else: len("_without_")
        )

      result += lcomma(g, n[1])
  of nkStaticTy:
    result =
      (if n.len > 0:
        lsub(g, n[0])
      else: 0
      ) + len("static[]")
  of nkTypeDef:
    result = lsons(g, n) + 3
  of nkOfInherit:
    result = lsub(g, n[0]) + len("of_")
  of nkProcTy:
    result = lsons(g, n) + len("proc")
  of nkIteratorTy:
    result = lsons(g, n) + len("iterator_")
  of nkSinkAsgn:
    result = lsons(g, n) + len("`=sink`(, )")
  of nkEnumTy:
    if n.len > 0:
      result = lsub(g, n[0]) + lcomma(g, n, 1) + len("enum_")
    else:
      result = len("enum")
  of nkEnumFieldDef:
    result = lsons(g, n) + 3
  of nkVarSection, nkLetSection:
    if n.len > 1:
      result = MaxLineLen + 1
    else:
      result = lsons(g, n) + len("var_")
  of nkUsingStmt:
    if n.len > 1:
      result = MaxLineLen + 1
    else:
      result = lsons(g, n) + len("using_")
  of nkReturnStmt:
    if n.len > 0 and n[0].kind == nkAsgn:
      result = len("return_") + lsub(g, n[0][1])
    else:
      result = len("return_") + lsub(g, n[0])
  of nkRaiseStmt:
    result = lsub(g, n[0]) + len("raise_")
  of nkYieldStmt:
    result = lsub(g, n[0]) + len("yield_")
  of nkDiscardStmt:
    result = lsub(g, n[0]) + len("discard_")
  of nkBreakStmt:
    result = lsub(g, n[0]) + len("break_")
  of nkContinueStmt:
    result = lsub(g, n[0]) + len("continue_")
  of nkPragma:
    result = lcomma(g, n) + len("_{..}")
  of nkCommentStmt:
    result = n.strVal.len
  of nkOfBranch:
    result = lcomma(g, n, 0, -2) + lsub(g, lastSon(n)) + len("of_:_")
  of nkImportAs:
    result = lsub(g, n[0]) + len("_as_") + lsub(g, n[1])
  of nkElifBranch:
    result = lsons(g, n) + len("elif_:_")
  of nkElse:
    result = lsub(g, n[0]) + len("else:_")
  of nkFinally:
    result = lsub(g, n[0]) + len("finally:_")
  of nkGenericParams:
    result = lcomma(g, n) + 2
  of nkFormalParams:
    result = lcomma(g, n, 1) + 2
    if n[0].kind != nkEmpty:
      result += lsub(g, n[0]) + 2
  of nkExceptBranch:
    result = lcomma(g, n, 0, -2) + lsub(g, lastSon(n)) + len("except_:_")
  of nkObjectTy:
    result = len("object_")
  else:
    result = MaxLineLen + 1

proc fits(g: TSrcGen; x: int): bool =
  result = x <= MaxLineLen

proc fits(x: int): bool =
  x <= MaxLineLen

proc overflows(g: TSrcGen; x: int): bool =
  not fits(g.lineLen + x)

proc gsub(g: var TSrcGen; n: PNode; flags: SubFlags = {}; fromStmtList = false)

proc putWithSpace(g: var TSrcGen; kind: TokType; s: string) =
  put(g, kind, s)
  put(g, tkSpaces, Space)

proc isHideable(config: ConfigRef; n: PNode): bool =
  # xxx compare `ident` directly with `getIdent(cache, wRaises)`, but
  # this requires a `cache`.
  case n.kind
  of nkExprColonExpr:
    result =
      n[0].kind == nkIdent and
        n[0].ident.s.nimIdentNormalize in
        ["raises", "tags", "extern", "deprecated", "forbids", "stacktrace"]
  of nkIdent:
    result = n.ident.s in ["gcsafe", "deprecated"]
  else:
    result = false

proc gextras(g: var TSrcGen; toks: openArray[Token]; indented: bool) =
  if toks.len == 0:
    return

  let long = toks.len > 1 or indented and overflows(g, len($toks[0]))
  if long:
    if indented:
      indentNL(g)
    else:
      optNL(g)

  for tok in toks:
    if tok.tokType == tkComment and g.lineLen > 0 and g.pendingNL < 0 and
        g.pendingWhitespace < 0:
      put(g, tkSpaces, " ")

    put(g, tok.tokType, $tok)
    optNL(g)

  if long and indented:
    dedent(g)

proc gprefixes(g: var TSrcGen; n: PNode) =
  gextras(g, n.prefix, false)

proc gmids(g: var TSrcGen; n: PNode) =
  gextras(g, n.mid, false)

proc gpostfixes(g: var TSrcGen; n: PNode) =
  # Postfixes are indented to increase chances that they stick
  # with the same node on re-parse since in most cases, we split
  # comments between prefix and postfix indent being greater than the
  # non-comment token that follows
  gextras(g, n.postfix, true)

proc eqIdent(n: PNode; s: string): bool =
  n.kind == nkIdent and n.ident != nil and n.ident.s.cmpIgnoreStyle(s) == 0

proc gcommaAux(
    g: var TSrcGen;
    n: PNode;
    ind: int;
    start: int = 0;
    theEnd: int = -1;
    separator = tkComma;
    flags: ListFlags = {};
    subFlags: SubFlags = {};
) =
  if start > n.len + theEnd:
    return

  let inPragma = g.inPragma == 1 # just the top-level

  var inHideable = false

  let oldInd = g.indent

  g.indent = ind
  # If a full, comma-separate list fits on one line, go for it. If not, we put
  # each element on its own line unless it's a list of trivial things (so as to
  # avoid wasting significant vertical space on lists of numbers and the like)
  let
    onePerLine =
      if fits(g, g.lineLen + lcomma(g, n, start, theEnd)):
        false
      else:
        anyIt(n.sons[start .. n.len + theEnd], not isSimple(it))
    firstSticky = lfFirstSticky in flags

  for i in start .. n.len + theEnd:
    let c = i < n.len + theEnd
    let sublen = lsub(g, n[i]) + ord(c)
    if not firstSticky or i > start:
      if onePerLine or overflows(g, sublen):
        optNL(g, ind)

    let oldLen = g.tokens.len
    if inPragma:
      if not inHideable and isHideable(g.config, n[i]):
        inHideable = true

        put(g, tkHideableStart, "")
      elif inHideable and not isHideable(g.config, n[i]):
        inHideable = false

        put(g, tkHideableEnd, "")

    gsub(g, n[i], flags = subFlags + {sfSkipPostfix})
    if c:
      if g.tokens.len > oldLen:
        if lfSkipPushComma notin flags or not eqIdent(n[i], "push"):
          putWithSpace(g, separator, $separator)
        else:
          put(g, tkSpaces, " ")

    gpostfixes(g, n[i])

  if lfSepAtEnd in flags or onePerLine and lfLongSepAtEnd in flags:
    put(g, separator, $separator)

  if inHideable:
    put(g, tkHideableEnd, "")

    inHideable = false

  g.indent = oldInd

proc gcomma(
    g: var TSrcGen;
    n: PNode;
    start: int = 0;
    theEnd: int = -1;
    indentNL = IndentWidth;
    flags: ListFlags = {};
) =
  var ind = g.indent + indentNL

  gcommaAux(g, n, ind, start, theEnd, flags = flags)

proc gsons(
    g: var TSrcGen; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
) =
  for i in start .. n.len + theEnd:
    gsub(g, n[i], flags)

proc gsonsNL(
    g: var TSrcGen; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
) =
  for i in start .. n.len + theEnd:
    gsub(g, n[i], flags)
    g.optNL()

proc glist(
    g: var TSrcGen;
    n: PNode;
    brOpen = tkParLe;
    separator = tkComma;
    extra = 0;
    start = 0;
    theEnd = -1;
    indentNL = IndentWidth;
    flags: ListFlags = {};
    subFlags: SubFlags = {};
) =
  # Render a list in the following preference order:
  # * If everything will fit on one line, including extra chars, do so
  # * If the list contents can fit on a single line, do so
  # * If the list contents are simple, use compact format
  # * Else use item-per-line format
  let brClose =
    case brOpen
    of tkParLe:
      tkParRi
    of tkBracketLe:
      tkBracketRi
    of tkCurlyLe:
      tkCurlyRi
    of tkBracketDotLe:
      tkBracketDotRi
    of tkCurlyDotLe:
      tkCurlyDotRi
    of tkParDotLe:
      tkParDotRi
    else:
      tkInvalid

  let
    brLen =
      (if brClose == tkInvalid:
        0
      else: len($brClose) * 2
      ) +
        (if lfSepAtEnd in flags:
          1
        else: 0
        )
    subLen = lcomma(g, n, start = start, theEnd = theEnd)
    ind = g.indent + indentNL
    withNL =
      n.len + theEnd >= start and overflows(g, subLen + brLen + extra) or n.mid.len > 0

  if brClose != tkInvalid:
    # TODO stack all opening brackets on one line if there are many
    put(g, brOpen, $brOpen)

  let oldInd = g.indent

  g.indent = ind
  if withNL:
    g.optNL()

  gmids(g, n)
  gcommaAux(g, n, ind, start, theEnd, separator, flags = flags, subFlags = subFlags)

  g.indent = oldInd
  if withNL:
    g.optNL(g.indent)

  if brClose != tkInvalid:
    put(g, brClose, $brClose)

proc gsection(g: var TSrcGen; n: PNode; kind: TokType; k: string) =
  if n.len == 0:
    return
  # empty var sections are possible
  putWithSpace(g, kind, k)
  if n.len > 1 or n.mid.len > 0 or n[0].prefix.len > 0:
    indentNL(g)
    gmids(g, n)
    for i in 0 ..< n.len:
      optNL(g)
      gsub(g, n[i])

    dedent(g)
  else:
    gsub(g, n[0])

proc gstmts(g: var TSrcGen; n: PNode; flags: SubFlags = {}; doIndent = true) =
  if n.kind == nkEmpty:
    gprefixes(g, n)
    if sfSkipPostfix notin flags:
      gpostfixes(g, n)

    return

  if doIndent:
    indentNL(g)

  let flags =
    if doIndent:
      flags + {sfNoIndent}
    else:
      flags
  # The original version tried to normalize newlines but this turned out to be
  # difficult to do whole-sale - we will thus retain some of the original
  # new-lines, normalizing only where a clear rule can be established
  if n.kind in {nkStmtList, nkStmtListExpr, nkStmtListType}:
    gprefixes(g, n)
    for i in 0 ..< n.len:
      # This groups sections by their kind, giving a bit of air between "parts"
      # of code - we don't do it before control flow because there, the code
      # before the control flow is often part of settting it up
      # TODO if the previous statement was complex (lots of lines / indent) we should
      #      probably add a newline even before control flow
      if i > 0:
        optNL(g, n[i - 1], n[i])
      # else:
      #   optNL(g)
      if n[i].kind in {nkStmtList, nkStmtListExpr, nkStmtListType}:
        gstmts(g, n[i], flags = flags, doIndent = false)
      else:
        gsub(g, n[i], fromStmtList = true)

    if sfSkipPostfix notin flags:
      gpostfixes(g, n)
  else:
    gsub(g, n, flags)

  if doIndent:
    dedent(g)

  optNL(g)

proc gcond(g: var TSrcGen; n: PNode; flags: SubFlags = {}) =
  if n.kind == nkStmtListExpr and not gsubAddsPar(n):
    put(g, tkParLe, "(")

  gsub(g, n, flags)
  if n.kind == nkStmtListExpr and not gsubAddsPar(n):
    put(g, tkParRi, ")")

proc gif(g: var TSrcGen; n: PNode; flags: SubFlags) =
  gprefixes(g, n[0])
  gcond(g, n[0][0], {sfLongIndent})
  putWithSpace(g, tkColon, ":")
  gmids(g, n[0])
  gsub(g, n[0][1])
  optNL(g)
  if sfSkipPostfix notin flags:
    gpostfixes(g, n[0])

  for i in 1 ..< n.len:
    optNL(g)
    gsub(g, n[i])

proc gwhile(g: var TSrcGen; n: PNode) =
  putWithSpace(g, tkWhile, "while")
  gcond(g, n[0], {sfLongIndent})
  putWithSpace(g, tkColon, ":")
  gmids(g, n)
  gstmts(g, n[1])

proc gpattern(g: var TSrcGen; n: PNode) =
  put(g, tkCurlyLe, "{")
  gmids(g, n)
  gstmts(g, n)
  put(g, tkCurlyRi, "}")

proc gpragmaBlock(g: var TSrcGen; n: PNode) =
  gsub(g, n[0])
  putWithSpace(g, tkColon, ":")
  gmids(g, n)
  gstmts(g, n[1])

proc gtry(g: var TSrcGen; n: PNode) =
  put(g, tkTry, "try")
  putWithSpace(g, tkColon, ":")
  gmids(g, n)
  gstmts(g, n[0])
  gsons(g, n, start = 1)

proc gfor(g: var TSrcGen; n: PNode) =
  putWithSpace(g, tkFor, "for")
  gcomma(g, n, start = 0, theEnd = -3)
  put(g, tkSpaces, Space)
  putWithSpace(g, tkIn, "in")
  gsub(g, n[^2], flags = {sfLongIndent})
  putWithSpace(g, tkColon, ":")
  gmids(g, n)
  gstmts(g, n[^1])

proc gcase(g: var TSrcGen; n: PNode) =
  if n.len == 0:
    return

  var last =
    if n[^1].kind == nkElse:
      -2
    else:
      -1

  putWithSpace(g, tkCase, "case")
  gcond(g, n[0])
  gmids(g, n)
  optNL(g)
  gsons(g, n, start = 1, theEnd = last)
  if last == -2:
    gsub(g, n[^1])

proc genSymSuffix(result: var string; s: PSym) {.inline.} =
  if sfGenSym in s.flags:
    result.add '_'
    result.addInt s.id

proc gproc(g: var TSrcGen; n: PNode) =
  if n[namePos].kind == nkSym:
    let s = n[namePos].sym

    var ret = renderDefinitionName(s)

    ret.genSymSuffix(s)
    put(g, tkSymbol, ret)
  else:
    gsub(g, n[namePos])

  if n[patternPos].kind != nkEmpty:
    gpattern(g, n[patternPos])
  # If there is no body, we don't need to indent the parameters as much
  let flags =
    if n[bodyPos].kind != nkEmpty:
      {sfLongIndent}
    else:
      {}

  gsub(g, n[genericParamsPos], flags)
  gsub(g, n[paramsPos], flags)
  gsub(g, n[pragmasPos], flags)
  if n[bodyPos].kind != nkEmpty:
    put(g, tkSpaces, Space)
    putWithSpace(g, tkEquals, "=")
    withIndent(g):
      gmids(g, n)

    gstmts(g, n[bodyPos], flags)
  else:
    withIndent(g):
      gmids(g, n)

proc gTypeClassTy(g: var TSrcGen; n: PNode) =
  putWithSpace(g, tkConcept, "concept")
  gsons(g, n[0]) # arglist
  gsub(g, n[1]) # pragmas
  gsub(g, n[2]) # of
  gmids(g, n)
  indentNL(g)
  gstmts(g, n[3])
  dedent(g)

proc gblock(g: var TSrcGen; n: PNode) =
  # you shouldn't simplify it to `n.len < 2`
  # because the following codes should be executed
  # even when block stmt has only one child for getting
  # better error messages.
  if n.len == 0:
    return

  if n[0].kind != nkEmpty:
    putWithSpace(g, tkBlock, "block")
    gsub(g, n[0])
  else:
    put(g, tkBlock, "block")
  # block stmt should have two children
  if n.len == 1:
    return

  putWithSpace(g, tkColon, ":")
  gmids(g, n)
  gstmts(g, n[1])

proc gstaticStmt(g: var TSrcGen; n: PNode) =
  put(g, tkStatic, "static")
  putWithSpace(g, tkColon, ":")
  gmids(g, n)
  gstmts(g, n[0])

proc gasm(g: var TSrcGen; n: PNode) =
  putWithSpace(g, tkAsm, "asm")
  gsub(g, n[0])
  if n.len > 1:
    gsub(g, n[1])

proc gident(g: var TSrcGen; n: PNode) =
  var t: TokType
  var s = atom(g, n)
  if s.len > 0 and s[0] in phlexer.SymChars:
    if n.kind == nkIdent:
      if (n.ident.id < ord(tokKeywordLow) - ord(tkSymbol)) or
          (n.ident.id > ord(tokKeywordHigh) - ord(tkSymbol)):
        t = tkSymbol
      else:
        t = TokType(n.ident.id + ord(tkSymbol))
    else:
      t = tkSymbol
  else:
    t = tkOpr

  put(g, t, s)

proc doParamsAux(g: var TSrcGen; params: PNode) =
  let retLen =
    if params.len > 0 and params[0].kind != nkEmpty:
      lsub(g, params[0]) + len(" -> ")
    else:
      0

  if params.len > 1:
    glist(g, params, tkParLe, tkSemiColon, extra = retLen, start = 1)

  if params.len > 0 and params[0].kind != nkEmpty:
    put(g, tkSpaces, Space)
    putWithSpace(g, tkOpr, "->")
    gsub(g, params[0])

proc gsubOptNL(
    g: var TSrcGen;
    n: PNode;
    indentNL = IndentWidth;
    fromStmtList = false;
    flags: SubFlags = {};
) =
  # Output n on the same line if it fits, else continue on next - indentation is
  # always set up in case a comment linebreaks the statement
  let nl = overflows(g, lsub(g, n))
  withIndent(g, indentNL):
    if nl:
      optNL(g)

    gsub(g, n, flags = flags, fromStmtList = fromStmtList)

proc skipHiddenNodes(n: PNode): PNode =
  result = n
  while result != nil:
    if result.kind in {nkHiddenStdConv, nkHiddenSubConv, nkHiddenCallConv} and
        result.len > 1:
      result = result[1]
    elif result.kind in
        {
          nkCheckedFieldExpr, nkHiddenAddr, nkHiddenDeref, nkStringToCString,
          nkCStringToString
        } and result.len > 0:
      result = result[0]
    else:
      break

proc accentedName(g: var TSrcGen; n: PNode) =
  # This is for cases where ident should've really been a `nkAccQuoted`, e.g. `:tmp`
  # or if user writes a macro with `ident":foo"`. It's unclear whether these should be legal.
  const backticksNeeded = OpChars + {'[', '{', '\''}
  if n == nil:
    return

  let ident = n.getPIdent
  if ident != nil and ident.s[0] in backticksNeeded:
    put(g, tkAccent, "`")
    gident(g, n)
    put(g, tkAccent, "`")
  else:
    gsub(g, n)

proc infixArgument(g: var TSrcGen; n: PNode; i: int; flags: SubFlags) =
  if i < 1 or i > 2:
    return

  var needsParenthesis = false

  let nNext = n[i].skipHiddenNodes
  if nNext.kind == nkInfix:
    if nNext[0].kind in {nkSym, nkIdent} and n[0].kind in {nkSym, nkIdent}:
      let nextId =
        if nNext[0].kind == nkSym:
          nNext[0].sym.name
        else:
          nNext[0].ident

      let nnId =
        if n[0].kind == nkSym:
          n[0].sym.name
        else:
          n[0].ident

      if i == 1:
        if getPrecedence(nextId) < getPrecedence(nnId):
          needsParenthesis = true
      elif i == 2:
        if getPrecedence(nextId) <= getPrecedence(nnId):
          needsParenthesis = true

  if needsParenthesis:
    put(g, tkParLe, "(")

  gcond(g, n[i], flags)
  if needsParenthesis:
    put(g, tkParRi, ")")

const postExprBlocks =
  {
    nkStmtList, nkStmtListExpr, nkOfBranch, nkElifBranch, nkElse, nkExceptBranch,
    nkFinally, nkDo
  }

proc postStatements(g: var TSrcGen; n: PNode; i: int; skipDo: bool) =
  var i = i
  if n[i].kind in {nkStmtList, nkStmtListExpr}:
    if skipDo:
      put(g, tkColon, ":")
    else:
      put(g, tkSpaces, Space)
      put(g, tkDo, "do")
      put(g, tkColon, ":")

  gsub(g, n[i])

  i.inc
  for j in i ..< n.len:
    if n[j].kind == nkDo:
      optNL(g)
    elif n[j].kind in {nkStmtList, nkStmtListExpr}:
      optNL(g)
      put(g, tkDo, "do")
      put(g, tkColon, ":")

    gsub(g, n[j])

proc isCustomLit(n: PNode): bool =
  if n.len == 2 and n[0].kind == nkRStrLit:
    let ident = n[1].getPIdent

    result = ident != nil and ident.s.startsWith('\'')

proc gsub(g: var TSrcGen; n: PNode; flags: SubFlags; fromStmtList = false) =
  if isNil(n):
    return

  if n.kind in {nkStmtList, nkStmtListExpr, nkStmtListType} and not gsubAddsPar(n):
    gstmts(g, n)

    return
  # When adding blanks after certain nodes, we only do so if there's a body
  let currLine = g.outputLine()
  gprefixes(g, n)

  case n.kind # atoms:
  of nkTripleStrLit:
    put(g, tkTripleStrLit, atom(g, n))
  of nkEmpty:
    discard
  of nkType:
    put(g, tkInvalid, atom(g, n))
  of nkSym, nkIdent:
    gident(g, n)
  of nkIntLit:
    put(g, tkIntLit, atom(g, n))
  of nkInt8Lit:
    put(g, tkInt8Lit, atom(g, n))
  of nkInt16Lit:
    put(g, tkInt16Lit, atom(g, n))
  of nkInt32Lit:
    put(g, tkInt32Lit, atom(g, n))
  of nkInt64Lit:
    put(g, tkInt64Lit, atom(g, n))
  of nkUIntLit:
    put(g, tkUIntLit, atom(g, n))
  of nkUInt8Lit:
    put(g, tkUInt8Lit, atom(g, n))
  of nkUInt16Lit:
    put(g, tkUInt16Lit, atom(g, n))
  of nkUInt32Lit:
    put(g, tkUInt32Lit, atom(g, n))
  of nkUInt64Lit:
    put(g, tkUInt64Lit, atom(g, n))
  of nkFloatLit:
    put(g, tkFloatLit, atom(g, n))
  of nkFloat32Lit:
    put(g, tkFloat32Lit, atom(g, n))
  of nkFloat64Lit:
    put(g, tkFloat64Lit, atom(g, n))
  of nkFloat128Lit:
    put(g, tkFloat128Lit, atom(g, n))
  of nkStrLit:
    put(g, tkStrLit, atom(g, n))
  of nkRStrLit:
    put(g, tkRStrLit, atom(g, n))
  of nkCharLit:
    put(g, tkCharLit, atom(g, n))
  of nkNilLit:
    put(g, tkNil, atom(g, n)) # complex expressions
  of nkCall, nkConv, nkDotCall, nkPattern, nkObjConstr:
    if n.len > 1 and n.lastSon.kind in postExprBlocks:
      accentedName(g, n[0])

      var i = 1
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc

      if i > 1:
        glist(
          g, n, tkParLe, start = 1, theEnd = i - 1 - n.len, flags = {lfLongSepAtEnd}
        )

      postStatements(g, n, i, fromStmtList)
    elif n.len >= 1:
      accentedName(g, n[0])
      glist(g, n, tkParLe, start = 1, flags = {lfLongSepAtEnd})
    else:
      put(g, tkParLe, "(")
      put(g, tkParRi, ")")
  of nkCallStrLit:
    if n.len > 0:
      accentedName(g, n[0])

    if n.len > 1 and n[1].kind == nkRStrLit:
      put(g, tkRStrLit, '\"' & replace(n[1].strVal, "\"", "\"\"") & '\"')
    else:
      gsub(g, n[1])
  of nkCast:
    put(g, tkCast, "cast")
    if n.len > 0 and n[0].kind != nkEmpty:
      put(g, tkBracketLe, "[")
      gsub(g, n[0])
      put(g, tkBracketRi, "]")

    put(g, tkParLe, "(")
    gsub(g, n[1])
    put(g, tkParRi, ")")
  of nkAddr:
    put(g, tkAddr, "addr")
    if n.len > 0:
      put(g, tkParLe, "(")
      gsub(g, n[0])
      put(g, tkParRi, ")")
  of nkStaticExpr:
    put(g, tkStatic, "static")
    put(g, tkSpaces, Space)
    gsub(g, n[0])
  of nkBracketExpr:
    gcond(g, n[0])
    put(g, tkBracketLe, "[")
    gcomma(g, n, 1, flags = {lfLongSepAtEnd})
    put(g, tkBracketRi, "]")
  of nkCurlyExpr:
    gcond(g, n[0])
    put(g, tkCurlyLe, "{")
    gcomma(g, n, 1)
    put(g, tkCurlyRi, "}")
  of nkPragmaExpr:
    gsub(g, n[0])
    gcomma(g, n, 1)
  of nkCommand:
    accentedName(g, n[0])
    put(g, tkSpaces, Space)
    if n.len > 1 and n.lastSon.kind in postExprBlocks:
      var i = 1
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc

      if i > 1:
        gcomma(g, n, 1, i - 1 - n.len, indentNL = IndentWidth, flags = {lfFirstSticky})
      # when parsing nkCommand, the compiler inserts `nkCall` to arguments if
      # ":" is present so it looks like we can skip the `do` here :/ this needs
      # deeper investigation - see also `nkPar` which sometimes removes the
      # parenthesis from the AST
      postStatements(g, n, i, true)
    else:
      # The first argument must not be line-broken, or command syntax breaks!
      if n.len > 1:
        gcomma(g, n, 1, indentNL = IndentWidth, flags = {lfFirstSticky})
  of nkExprEqExpr, nkAsgn, nkFastAsgn:
    gsub(g, n[0])
    put(g, tkSpaces, Space)
    putWithSpace(g, tkEquals, "=")
    gmids(g, n)
    gsubOptNL(g, n[1])
  of nkPar, nkClosure:
    glist(g, n, tkParLe, subflags = {sfNoIndent})
  of nkTupleConstr:
    let flags =
      if n.len == 1 and n[0].kind != nkExprColonExpr:
        {lfSepAtEnd}
      else:
        {lfLongSepAtEnd}

    glist(g, n, tkParLe, flags = flags)
  of nkCurly:
    glist(g, n, tkCurlyLe)
  of nkArgList:
    glist(g, n, tkInvalid)
  of nkTableConstr:
    if n.len > 0:
      glist(g, n, tkCurlyLe)
    else:
      put(g, tkCurlyLe, "{")
      put(g, tkColon, ":")
      put(g, tkCurlyRi, "}")
  of nkBracket:
    glist(g, n, tkBracketLe)
  of nkDotExpr:
    if isCustomLit(n):
      put(g, tkCustomLit, n[0].strVal)
      gmids(g, n)
      gsub(g, n[1])
    else:
      gsub(g, n[0])
      # Mids here are put on a new line, then the dot follows on yet another new
      # line as dot parsing continues after a comment on a new line too! see
      # clMid pickup point in phparser.dotExpr
      if n.mid.len > 0:
        g.optNL()
        gmids(g, n)
      put(g, tkDot, ".")

      assert n.len == 2, $n.len

      accentedName(g, n[1])
  of nkBind:
    putWithSpace(g, tkBind, "bind")
    gsub(g, n[0])
  of nkLambda:
    putWithSpace(g, tkProc, "proc")
    gsub(g, n[paramsPos])
    gsub(g, n[pragmasPos])
    put(g, tkSpaces, Space)
    putWithSpace(g, tkEquals, "=")
    gmids(g, n)
    gsubOptNL(g, n[bodyPos])
  of nkDo:
    putWithSpace(g, tkDo, " do") # TODO space here is ugly
    if paramsPos < n.len:
      doParamsAux(g, n[paramsPos])

    gsub(g, n[pragmasPos])
    put(g, tkColon, ":")
    gmids(g, n)
    gsub(g, n[bodyPos])
  of nkIdentDefs:
    gcomma(g, n, 0, -3, indentNL = 0)
    if n.len >= 2 and n[^2].kind != nkEmpty:
      putWithSpace(g, tkColon, ":")
      gsubOptNL(g, n[^2])

    if n.len >= 1 and n[^1].kind != nkEmpty:
      put(g, tkSpaces, Space)
      putWithSpace(g, tkEquals, "=")
      gsubOptNL(g, n[^1], fromStmtList = true)
  of nkConstDef:
    gcomma(g, n, 0, -3)
    if n.len >= 2 and n[^2].kind != nkEmpty:
      putWithSpace(g, tkColon, ":")
      gsub(g, n[^2])

    if n.len >= 1 and n[^1].kind != nkEmpty:
      put(g, tkSpaces, Space)
      putWithSpace(g, tkEquals, "=")
      gsubOptNL(g, n[^1])
  of nkVarTuple:
    if n[^1].kind == nkEmpty:
      glist(g, n, tkParLe, theEnd = -2)
    else:
      glist(g, n, tkParLe, extra = len(" = "), theEnd = -3)
      put(g, tkSpaces, Space)
      putWithSpace(g, tkEquals, "=")
      gsubOptNL(g, n[^1])
  of nkExprColonExpr:
    gsub(g, n[0])
    putWithSpace(g, tkColon, ":")
    gsubOptNL(g, n[1])
  of nkInfix:
    if n.len < 3:
      put(g, tkOpr, "Too few children for nkInfix")

      return

    infixArgument(g, n, 1, flags = flags)

    let spaces = g.inImportLike == 0 or n[0].kind != nkIdent or n[0].ident.s != "/"
    if spaces:
      put(g, tkSpaces, Space)

    gsub(g, n[0], flags = flags) # binary operator

    let
      overflows = n.len == 3 and overflows(g, lsub(g, n[2]))
      indent = overflows and sfNoIndent notin flags
      wid =
        if sfLongIndent in flags:
          longIndentWid
        else:
          IndentWidth
      flags =
        if indent:
          # Only indent infix once otherwise for long strings of + / and / etc
          # we get a cascade
          flags + {sfNoIndent}
        else:
          flags

    if indent:
      indentNL(g, wid)
    elif overflows:
      optNL(g)
    elif spaces:
      put(g, tkSpaces, Space)

    infixArgument(g, n, 2, flags = flags)
    if indent:
      dedent(g, wid)

    if n.len > 3 and n.lastSon.kind in postExprBlocks:
      var i = 3
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc

      postStatements(g, n, i, fromStmtList)
  of nkPrefix:
    gsub(g, n[0])
    if n.len > 1:
      let opr =
        if n[0].kind == nkIdent:
          n[0].ident
        elif n[0].kind == nkSym:
          n[0].sym.name
        elif n[0].kind in {nkOpenSymChoice, nkClosedSymChoice}:
          n[0][0].sym.name
        else:
          nil

      let nNext = skipHiddenNodes(n[1])
      if nNext.kind == nkPrefix or (opr != nil and phrenderer.isKeyword(opr)):
        put(g, tkSpaces, Space)

      if nNext.kind == nkInfix:
        put(g, tkParLe, "(")
        gsub(g, n[1])
        put(g, tkParRi, ")")
      else:
        gsub(g, n[1])

    if n.len > 2 and n.lastSon.kind in postExprBlocks:
      var i = 2
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc

      postStatements(g, n, i, fromStmtList)
  of nkPostfix:
    gsub(g, n[1])
    gsub(g, n[0])
  of nkRange:
    gsub(g, n[0])
    put(g, tkDotDot, "..")
    gsub(g, n[1])
  of nkDerefExpr:
    gsub(g, n[0])
    put(g, tkOpr, "[]")
  of nkAccQuoted:
    put(g, tkAccent, "`")
    for i in 0 ..< n.len:
      proc isAlpha(n: PNode): bool =
        if n.kind in {nkIdent, nkSym}:
          let tmp = n.getPIdent.s

          result = tmp.len > 0 and tmp[0] in {'a' .. 'z', 'A' .. 'Z'}

      var useSpace = false
      if i == 1 and n[0].kind == nkIdent and n[0].ident.s in ["=", "'"]:
        if not n[1].isAlpha: # handle `=destroy`, `'big'
          useSpace = true
      elif i == 1 and n[1].kind == nkIdent and n[1].ident.s == "=":
        if not n[0].isAlpha: # handle setters, e.g. `foo=`
          useSpace = true
      elif i > 0:
        useSpace = true

      if useSpace:
        put(g, tkSpaces, Space)

      gsub(g, n[i])

    put(g, tkAccent, "`")
  of nkIfExpr:
    putWithSpace(g, tkIf, "if")
    if n.len > 0:
      gcond(g, n[0][0])

    putWithSpace(g, tkColon, ":")
    if n.len > 0:
      gmids(g, n[0])
      gsub(g, n[0][1])
      optNL(g)

    gsons(g, n, 1)
  of nkElifExpr:
    putWithSpace(g, tkElif, "elif")
    gcond(g, n[0])
    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gsub(g, n[1])
    optNL(g)
  of nkElseExpr:
    put(g, tkElse, "else")
    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gsub(g, n[0])
    optNL(g)
  of nkTypeOfExpr:
    put(g, tkType, "typeof")
    put(g, tkParLe, "(")
    if n.len > 0:
      gsub(g, n[0])

    put(g, tkParRi, ")")
  of nkRefTy:
    if n.len > 0:
      putWithSpace(g, tkRef, "ref")
      gsub(g, n[0])
    else:
      put(g, tkRef, "ref")
  of nkPtrTy:
    if n.len > 0:
      putWithSpace(g, tkPtr, "ptr")
      gsub(g, n[0])
    else:
      put(g, tkPtr, "ptr")
  of nkVarTy:
    if n.len > 0:
      putWithSpace(g, tkVar, "var")
      gsub(g, n[0])
    else:
      put(g, tkVar, "var")
  of nkOutTy:
    if n.len > 0:
      putWithSpace(g, tkOut, "out")
      gsub(g, n[0])
    else:
      put(g, tkOut, "out")
  of nkDistinctTy:
    if n.len > 0:
      putWithSpace(g, tkDistinct, "distinct")
      gsub(g, n[0])
      if n.len > 1:
        if n[1].kind == nkWith:
          putWithSpace(g, tkSymbol, " with")
        else:
          putWithSpace(g, tkSymbol, " without")

        gcomma(g, n[1])
    else:
      put(g, tkDistinct, "distinct")
  of nkTypeDef:
    if n[0].kind == nkPragmaExpr:
      # generate pragma after generic
      gsub(g, n[0][0])
      if n[0].postfix.len > 0:
        g.indentNL()

      gsub(g, n[1])
      gsub(g, n[0][1])
    else:
      gsub(g, n[0])
      if n[0].postfix.len > 0:
        g.indentNL()

      gsub(g, n[1])

    put(g, tkSpaces, Space)
    if n.len > 2 and n[2].kind != nkEmpty:
      putWithSpace(g, tkEquals, "=")
      if n[2].kind in {nkObjectTy, nkEnumTy, nkRefTy}:
        gsub(g, n[2])
      else:
        gsubOptNL(g, n[2])

    if n[0].postfix.len > 0:
      g.dedent()
  of nkObjectTy:
    if n.len > 0:
      putWithSpace(g, tkObject, "object")
      gsub(g, n[0]) # nkEmpty (unused)
      gsub(g, n[1]) # nkOfInherit / nkEmpty
      withIndent(g):
        gmids(g, n)

      gsub(g, n[2]) # fields
    else:
      put(g, tkObject, "object")
  of nkRecList:
    if sfNoIndent notin flags:
      indentNL(g)

    gmids(g, n)
    for i in 0 ..< n.len:
      # The prefix check is here because if the previous line has a postfix
      # and the next has a prefix and they are both doc comments,
      # this might generate an invalid doc comment
      # TODO this should only apply to doc comments and maybe needs deep
      # comment inspection
      if i > 0 and n[i].prefix.len == 0:
        optNL(g, n[i - 1], n[i])
      else:
        optNL(g)
      gsub(g, n[i])

    if sfNoIndent notin flags:
      dedent(g)
  of nkOfInherit:
    putWithSpace(g, tkOf, "of")
    gsub(g, n[0])
  of nkProcTy:
    if n.len > 0:
      put(g, tkProc, "proc")
      gsub(g, n[0])
      gsub(g, n[1])
    else:
      put(g, tkProc, "proc")
  of nkIteratorTy:
    if n.len > 0:
      putWithSpace(g, tkIterator, "iterator")
      gsub(g, n[0])
      gsub(g, n[1])
    else:
      put(g, tkIterator, "iterator")
  of nkStaticTy:
    put(g, tkStatic, "static")
    put(g, tkBracketLe, "[")
    if n.len > 0:
      gsub(g, n[0])

    put(g, tkBracketRi, "]")
  of nkEnumTy:
    if n.len > 0:
      putWithSpace(g, tkEnum, "enum")
      gsub(g, n[0])
      withIndent(g):
        gmids(g, n)

      indentNL(g)
      gsonsNL(g, n, 1)
      dedent(g)
    else:
      put(g, tkEnum, "enum")
  of nkEnumFieldDef:
    gsub(g, n[0])
    put(g, tkSpaces, Space)
    putWithSpace(g, tkEquals, "=")
    gsub(g, n[1])
  of nkStmtList, nkStmtListExpr, nkStmtListType:
    if gsubAddsPar(n):
      put(g, tkParLe, "(")
      gsub(g, n[0])
      put(g, tkParRi, ")")
    else:
      raiseAssert "Handled above with gstmts"
  of nkIfStmt:
    putWithSpace(g, tkIf, "if")
    gif(g, n, flags)
  of nkWhen, nkRecWhen:
    putWithSpace(g, tkWhen, "when")
    gif(g, n, flags)
  of nkWhileStmt:
    gwhile(g, n)
  of nkPragmaBlock:
    gpragmaBlock(g, n)
  of nkCaseStmt, nkRecCase:
    gcase(g, n)
  of nkTryStmt, nkHiddenTryStmt:
    gtry(g, n)
  of nkForStmt, nkParForStmt:
    gfor(g, n)
  of nkBlockStmt, nkBlockExpr:
    gblock(g, n)
  of nkStaticStmt:
    gstaticStmt(g, n)
  of nkAsmStmt:
    gasm(g, n)
  of nkProcDef:
    putWithSpace(g, tkProc, "proc")
    gproc(g, n)
  of nkFuncDef:
    putWithSpace(g, tkFunc, "func")
    gproc(g, n)
  of nkConverterDef:
    putWithSpace(g, tkConverter, "converter")
    gproc(g, n)
  of nkMethodDef:
    putWithSpace(g, tkMethod, "method")
    gproc(g, n)
  of nkIteratorDef:
    putWithSpace(g, tkIterator, "iterator")
    gproc(g, n)
  of nkMacroDef:
    putWithSpace(g, tkMacro, "macro")
    gproc(g, n)
  of nkTemplateDef:
    putWithSpace(g, tkTemplate, "template")
    gproc(g, n)
  of nkTypeSection:
    gsection(g, n, tkType, "type")
  of nkConstSection:
    gsection(g, n, tkConst, "const")
  of nkVarSection:
    gsection(g, n, tkVar, "var")
  of nkLetSection:
    gsection(g, n, tkLet, "let")
  of nkUsingStmt:
    gsection(g, n, tkUsing, "using")
  of nkReturnStmt:
    putWithSpace(g, tkReturn, "return")
    withIndent(g):
      gmids(g, n)
      if n.len > 0 and n[0].kind == nkAsgn:
        gsub(g, n[0][1])
      else:
        gsub(g, n[0])
  of nkRaiseStmt:
    putWithSpace(g, tkRaise, "raise")
    withIndent(g):
      gmids(g, n)
      gsub(g, n[0])
  of nkYieldStmt:
    putWithSpace(g, tkYield, "yield")
    withIndent(g):
      gmids(g, n)
      gsub(g, n[0])
  of nkDiscardStmt:
    putWithSpace(g, tkDiscard, "discard")
    withIndent(g):
      gmids(g, n)
      gsub(g, n[0])
  of nkBreakStmt:
    putWithSpace(g, tkBreak, "break")
    withIndent(g):
      gmids(g, n)
      gsub(g, n[0])
  of nkContinueStmt:
    putWithSpace(g, tkContinue, "continue")
    withIndent(g):
      gmids(g, n)
      gsub(g, n[0])
  of nkPragma:
    if g.inPragma <= 0:
      inc g.inPragma

      put(g, tkSpaces, Space)

      let indentWid =
        if sfLongIndent in flags:
          longIndentWid
        else:
          IndentWidth

      glist(g, n, tkCurlyDotLe, indentNL = indentWid, flags = {lfSkipPushComma})

      dec g.inPragma
    else:
      gcomma(g, n)
  of nkImportStmt, nkExportStmt:
    g.inImportLike += 1
    if n.kind == nkImportStmt:
      putWithSpace(g, tkImport, "import")
    else:
      putWithSpace(g, tkExport, "export")

    glist(g, n, brOpen = tkInvalid)

    g.inImportLike -= 1
  of nkImportExceptStmt, nkExportExceptStmt:
    g.inImportLike += 1
    if n.kind == nkImportExceptStmt:
      putWithSpace(g, tkImport, "import")
    else:
      putWithSpace(g, tkExport, "export")

    gsub(g, n[0])
    put(g, tkSpaces, Space)
    putWithSpace(g, tkExcept, "except")
    gcommaAux(g, n, g.indent, 1)

    g.inImportLike -= 1
  of nkFromStmt:
    g.inImportLike += 1

    putWithSpace(g, tkFrom, "from")
    gsub(g, n[0])
    put(g, tkSpaces, Space)
    putWithSpace(g, tkImport, "import")
    glist(g, n, brOpen = tkInvalid, start = 1)

    g.inImportLike -= 1
  of nkIncludeStmt:
    g.inImportLike += 1

    putWithSpace(g, tkInclude, "include")
    glist(g, n, brOpen = tkInvalid)

    g.inImportLike -= 1
  of nkCommentStmt:
    putComment(g, n.strVal)
  of nkOfBranch:
    optNL(g)
    putWithSpace(g, tkOf, "of")
    gcomma(g, n, 0, -2, indentNL = longIndentWid)
    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gstmts(g, lastSon(n))
  of nkImportAs:
    gsub(g, n[0])
    put(g, tkSpaces, Space)
    putWithSpace(g, tkAs, "as")
    gsub(g, n[1])
  of nkBindStmt:
    putWithSpace(g, tkBind, "bind")
    gcomma(g, n)
  of nkMixinStmt:
    putWithSpace(g, tkMixin, "mixin")
    gcomma(g, n)
  of nkElifBranch:
    optNL(g)
    putWithSpace(g, tkElif, "elif")
    gcond(g, n[0], flags = {sfLongIndent})
    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gstmts(g, n[1])
  of nkElse:
    optNL(g)
    put(g, tkElse, "else")
    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gstmts(g, n[0])
  of nkFinally, nkDefer:
    optNL(g)
    if n.kind == nkFinally:
      put(g, tkFinally, "finally")
    else:
      put(g, tkDefer, "defer")

    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gstmts(g, n[0])
  of nkExceptBranch:
    optNL(g)
    if n.len != 1:
      putWithSpace(g, tkExcept, "except")
    else:
      put(g, tkExcept, "except")

    gcomma(g, n, 0, -2, indentNL = longIndentWid)
    putWithSpace(g, tkColon, ":")
    gmids(g, n)
    gstmts(g, lastSon(n))
  of nkGenericParams:
    proc hasExplicitParams(gp: PNode): bool =
      for p in gp:
        if p.typ == nil or tfImplicitTypeParam notin p.typ.flags:
          return true

      return false

    if n.hasExplicitParams:
      glist(
        g,
        n,
        tkBracketLe,
        tkSemiColon,
        indentNL =
          if sfLongIndent in flags:
            longIndentWid
          else:
            IndentWidth
          ,
      )
  of nkFormalParams:
    # Need to add empty parens here, or nkProcTy parsing becomes non-equal -
    # see hasSignature - it would be nicer to remove them when unncessary
    let indentWid =
      if sfLongIndent in flags:
        longIndentWid
      else:
        IndentWidth

    if n.len >= 1:
      let retExtra =
        if n.len > 0 and n[0].kind != nkEmpty:
          len(": ") + lsub(g, n[0])
        else:
          0
      # Semi-colon here is ugly but necessary for semantic equality, else we
      # get different groupings of nkIdentDefs and their descendants
      # TODO relaxing this to semantic equivalence would allow the use of `,`
      #      sometimes - nobody really wants `;`
      # Properties of the proc formal params formatting:
      # * long indent when body follows to separate args from body
      # * separator at end when using one-per-line for easy copy-pasting and
      #   smaller git diffs
      glist(
        g,
        n,
        tkParLe,
        tkSemiColon,
        extra = retExtra,
        start = 1,
        indentNL = indentWid,
        flags = {lfLongSepAtEnd},
      )

    if n.len > 0 and n[0].kind != nkEmpty:
      putWithSpace(g, tkColon, ":")
      gsubOptNL(g, n[0], indentNL = indentWid)
  of nkTupleTy:
    put(g, tkTuple, "tuple")
    glist(g, n, tkBracketLe)
  of nkTupleClassTy:
    put(g, tkTuple, "tuple")
  of nkTypeClassTy:
    gTypeClassTy(g, n)
  of nkError:
    putWithSpace(g, tkSymbol, "error")
    gsub(g, n[0])
  else:
    #nkNone, nkExplicitTypeListCall:
    internalError(g.config, n.info, "renderer.gsub(" & $n.kind & ')')

  if sfSkipPostfix notin flags:
    gpostfixes(g, n)

  if n.kind in blankAfterComplex and currLine < g.line:
    g.blankLine()

proc renderTree*(n: PNode; conf: ConfigRef = nil): string =
  if n == nil:
    return "<nil tree>"

  var g: TSrcGen

  initSrcGen(
    g,
    if conf == nil:
      newPartialConfigRef()
    else:
      conf
    ,
  )
  # do not indent the initial statement list so that
  # writeFile("file.nim", repr n)
  # produces working Nim code:
  if n.kind in {nkStmtList, nkStmtListExpr, nkStmtListType}:
    gstmts(g, n, doIndent = false)
  else:
    gsub(g, n)

  result = g.buf

proc `$`*(n: PNode): string =
  n.renderTree
