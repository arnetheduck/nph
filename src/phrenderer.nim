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
  blankAfterComplex = {nkObjectTy, nkEnumTy, nkTypeSection, nkProcDef..nkIteratorDef}
    ## If a statment is sufficiently complex as measured by the number of lines
    ## it occupies, add a blank line after it
  subOptSkipNL = {
    nkPar, nkCurly, nkBracket, nkTableConstr, nkStmtListExpr, nkPragma, nkPragmaExpr
  }
    ## When rendering nodes of the above kind, ignore length of children for the purpose
    ## of adding newlines
  Space = " "

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
    inConcept: int
    pendingNewline: bool
    fid*: FileIndex
    config*: ConfigRef
    mangler: seq[PSym]

  TSrcLen = object
    # A "fake" source generator that counts line lengths instead of outputting them
    indent*: int
    lineLen*: int
    line: int # The line where a token was last output
    pendingWhitespace: int
    inPragma: int
    inImportLike*: int
    inConcept: int
    tokens*: TRenderTokSeq
    config*: ConfigRef
    fid: FileIndex

  ListFlag = enum
    lfFirstSticky
      ## Render the first item of a list on the same line even if it doesn't fit
    lfFirstAlone ## Render the first sticky item alone if the rest doesn't fit
    lfSepAtEnd ## Always add separator at end
    lfLongSepAtEnd ## Add separator at end in one-per-line mode
    lfSkipPushComma ## Hack to remove first comma in pragma push
    lfFirstCommentSticky ## Render the first comment on the same line if line-breaking

  ListFlags = set[ListFlag]
  SubFlag = enum
    sfNoIndent ## Already performed the indent for this sub (usually an infix)
    sfLongIndent ## We're about to hit an indented section so make a double-indent
    sfSkipPostfix ## the caller will handle rendering the postfix
    sfSkipDo ## Don't add `do` token for post-statments

  SubFlags = set[SubFlag]
  TOutput = TSrcGen | TSrcLen

proc flagIndent(flags: SubFlags): int =
  if sfNoIndent in flags:
    0
  elif sfLongIndent in flags:
    longIndentWid
  else:
    IndentWidth

proc renderTree*(n: PNode; conf: ConfigRef = nil): string

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

proc hasComments(n: PNode): bool =
  if n.kind == nkCommentStmt or n.prefix.len > 0 or n.mid.len > 0 or n.postfix.len > 0:
    true
  else:
    case n.kind
    of
        nkCharLit..nkUInt64Lit,
        nkFloatLit..nkFloat128Lit,
        nkStrLit..nkTripleStrLit,
        nkIdent:
      false
    else:
      n.anyIt(hasComments(it))

proc isSimple(n: PNode; allowExported = false; allowInfix = false): bool =
  ## Simple nodes are those that are either literals, identifiers or simple
  ## lists thereof - we will stack these up when rendering lists
  if hascomments(n):
    false
  else:
    case n.kind
    of nkEmpty, nkCharLit..nkNilLit, nkIdent, nkAccQuoted:
      true
    of nkStmtList, nkImportStmt, nkExportStmt:
      n.allIt(isSimple(it))
    of nkIdentDefs:
      # Simple when it's just a name, potentially exported
      n.allIt(isSimple(it, true)) and n[^1].kind == nkEmpty and n[^2].kind == nkEmpty
    of nkPostfix:
      allowExported and isExported(n)
    of nkInfix:
      allowInfix and n.allIt(isSimple(it, allowInfix = allowInfix))
    of nkObjectTy:
      # Objects declarations without fields
      n.len == 0 or n.allIt(isSimple(it)) and n[2].len == 0
    of nkTypeDef, nkRefTy, nkOfInherit, nkGenericParams:
      n.allIt(isSimple(it, allowInfix = true))
    of nkDiscardStmt:
      n.allIt(isSimple(it))
    of nkProcTy:
      true
    else:
      false

# We render the source code in a two phases: The first
# determines how long the subtree will likely be, the second
# phase appends to a buffer that will be the output.
proc isKeyword*(i: PIdent): bool =
  if (i.id >= ord(tokKeywordLow) - ord(tkSymbol)) and (
    i.id <= ord(tokKeywordHigh) - ord(tkSymbol)
  ):
    result = true

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

proc containsNL(s: string): bool =
  for i in 0..<s.len:
    case s[i]
    of '\r', '\n':
      return true
    else:
      discard

  result = false

proc addTok(g: var TSrcLen; kind: TokType; s: string) =
  g.tokens.add TRenderTok(kind: kind, length: int16(s.len))

proc addPendingNL(g: var TSrcLen) =
  if g.pendingWhitespace >= 1:
    if g.lineLen > g.pendingWhitespace:
      addTok(g, tkSpaces, spaces(g.pendingWhitespace))

    g.pendingWhitespace = -1

proc put(g: var TSrcLen; kind: TokType; s: string) =
  # debugEcho "put ", kind, " ", s
  if kind != tkSpaces:
    addPendingNL(g)
    if s.len > 0 or kind in {tkHideableStart, tkHideableEnd}:
      addTok(g, kind, s)
  else:
    g.pendingWhitespace = s.len

  if containsNL(s):
    g.lineLen = MaxLineLen + 1
  else:
    g.lineLen += s.len

proc optNL(g: var TSrcLen) =
  g.lineLen = MaxLineLen + 1

proc optNL(g: var TSrcLen; a, b: PNode) =
  g.lineLen = MaxLineLen + 1

proc addTok(g: var TSrcGen; kind: TokType; s: string) =
  # debugEcho "addTok ", kind, " " , s.len, " ", s
  g.tokens.add TRenderTok(kind: kind, length: int16(s.len))

  g.buf.add(s)
  g.line += count(s, "\n")

proc outputLine(g: TSrcGen): int =
  ## The line it would be added if we were to add a token
  g.line + (
    if g.pendingNL >= 0:
      1 + ord(g.pendingNewline)
    else:
      0
  )

proc outputLine(g: TSrcLen): int =
  0

proc addPendingNL(g: var TSrcGen) =
  if g.pendingNL >= 0:
    let newlines = repeat("\n", 1 + ord(g.pendingNewline))
    g.pendingNewline = false

    addTok(g, tkSpaces, newlines & spaces(g.pendingNL))

    g.lineLen = g.pendingNL
    g.pendingNL = -1
    g.pendingWhitespace = -1
  elif g.pendingWhitespace >= 1:
    if g.lineLen > g.pendingWhitespace:
      addTok(g, tkSpaces, spaces(g.pendingWhitespace))

    g.pendingWhitespace = -1

proc optNL(g: var TSrcGen; indent: int) =
  g.pendingNL = indent
  g.lineLen = indent

proc optNL(g: var TSrcGen) =
  optNL(g, g.indent)

proc optNL(g: var TSrcGen; a, b: PNode) =
  g.pendingNL = g.indent
  g.lineLen = g.indent

  let
    endLine =
      if a.postfix.len > 0:
        a.postfix[^1].lineB
      else:
        int a.endInfo.line
    startLine =
      if b.prefix.len > 0:
        b.prefix[0].line
      else:
        int b.info.line

  g.pendingNewline = g.pendingNewline or endLine + 1 < startLine

proc blankLine(g: var TSrcGen; v = true) =
  g.pendingNewline = v or g.pendingNewline

proc optIndent(g: var TSrcGen; wid = IndentWidth) =
  # Set up an indent increase, if a newline were to happen
  g.indent += wid
  if g.pendingNL >= 0:
    g.pendingNL = g.indent
    g.lineLen = g.indent

proc optIndent(g: var TSrcLen; wid = IndentWidth) =
  g.indent += wid

proc indentNL(g: var TSrcGen; wid = IndentWidth) =
  g.indent += wid
  g.pendingNL = g.indent
  g.lineLen = g.indent

proc dedent(g: var TSrcGen; wid = IndentWidth) =
  g.indent -= wid

  assert(g.indent >= 0)
  if g.pendingNL > wid:
    g.pendingNL -= wid
    g.lineLen -= wid

proc blankLine(g: var TSrcLen; v = true) =
  discard

proc indentNL(g: var TSrcLen; wid = IndentWidth) =
  g.indent += wid

proc dedent(g: var TSrcLen; wid = IndentWidth) =
  g.indent -= wid

template withIndent(g: var TOutput; indentParam: int; body: untyped) =
  g.indent += indentParam

  body

  g.indent -= indentParam

template withIndent(g: var TOutput; body: untyped) =
  withIndent(g, IndentWidth, body)

proc put(g: var TSrcGen; kind: TokType; s: string) =
  # debugEcho "put ", kind, " ", s
  if kind != tkSpaces:
    addPendingNL(g)
    if s.len > 0 or kind in {tkHideableStart, tkHideableEnd}:
      addTok(g, kind, s)
  else:
    g.pendingWhitespace = s.len

  inc(g.lineLen, s.len)

proc optSpace(g: var TOutput) =
  put(g, tkSpaces, Space)

proc putComment(g: var TSrcGen; s: string) =
  if s.len == 0:
    return

  optSpace(g)
  put(g, tkComment, s)

  # TODO no implied eol on multiline comments
  optNL(g)

proc putComment(g: var TSrcLen; s: string) =
  if s.len == 0:
    return

  g.lineLen = MaxLineLen + 1

proc brCloseOf(brOpen: TokType): TokType =
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

proc skipHiddenNodes(n: PNode): PNode =
  result = n
  while result != nil:
    if result.kind in {nkHiddenStdConv, nkHiddenSubConv, nkHiddenCallConv} and
        result.len > 1:
      result = result[1]
    elif result.kind in {
      nkCheckedFieldExpr, nkHiddenAddr, nkHiddenDeref, nkStringToCString,
      nkCStringToString
    } and result.len > 0:
      result = result[0]
    else:
      break

proc infixHasParens(n: PNode; i: int): bool =
  let nNext = n[i].skipHiddenNodes
  if nNext.kind == nkInfix:
    if nNext[0].kind in {nkSym, nkIdent} and n[0].kind in {nkSym, nkIdent}:
      let
        nextId =
          if nNext[0].kind == nkSym:
            nNext[0].sym.name
          else:
            nNext[0].ident

      let
        nnId =
          if n[0].kind == nkSym:
            n[0].sym.name
          else:
            n[0].ident

      if i == 1:
        if getPrecedence(nextId) < getPrecedence(nnId):
          return true
      elif i == 2:
        if getPrecedence(nextId) <= getPrecedence(nnId):
          return true
  false

proc hasIndent(n: PNode): bool =
  n.kind in {
    nkPar, nkCurly, nkBracket, nkTableConstr, nkStmtListExpr, nkPragma, nkPragmaExpr,
    nkObjectTy, nkEnumTy
  }

proc litAux(g: TOutput; n: PNode; x: BiggestInt; size: int): string =
  proc skip(t: PType): PType =
    result = t
    while result != nil and
        result.kind in {
          tyGenericInst, tyRange, tyVar, tyLent, tyDistinct, tyOrdinal, tyAlias, tySink
        }:
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
    var
      y =
        if size < sizeof(BiggestInt):
          x and ((1.BiggestInt shl (size * 8)) - 1)
        else:
          x

    result = "0o" & toOct(y, size * 3)
  elif nfBase16 in n.flags:
    result = "0x" & toHex(x, size * 2)
  else:
    result = $x

proc ulitAux(g: TOutput; n: PNode; x: BiggestInt; size: int): string =
  if nfBase2 in n.flags:
    result = "0b" & toBin(x, size * 8)
  elif nfBase8 in n.flags:
    result = "0o" & toOct(x, size * 3)
  elif nfBase16 in n.flags:
    result = "0x" & toHex(x, size * 2)
  else:
    result = $cast[BiggestUInt](x)

proc atom(g: TOutput; n: PNode): string =
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

proc init(T: type TSrcLen; g: TSrcGen): T =
  T(config: g.config, fid: g.fid, pendingWhitespace: -1)

proc gsub(g: var TOutput; n: PNode; flags: SubFlags = {})
proc gsons(
  g: var TOutput; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
)

proc gcomma(
  g: var TOutput;
  n: PNode;
  start: int = 0;
  theEnd: int = -1;
  separator = tkComma;
  indentNL: int = IndentWidth;
  flags: ListFlags = {};
  subFlags: SubFlags = {};
)

proc glist(
  g: var TOutput;
  n: PNode;
  brOpen = tkParLe;
  separator = tkComma;
  extra = 0;
  start = 0;
  theEnd = -1;
  indentNL = IndentWidth;
  flags: ListFlags = {};
  subFlags: SubFlags = {};
)

proc lsub(g: TSrcGen; n: PNode; flags: SubFlags = {}): int =
  var g = TSrcLen.init(g)
  gsub(g, n, flags)
  g.lineLen

proc lsub(g: TSrcLen; n: PNode; flags: SubFlags = {}): int =
  0

proc lsons(
    g: var TSrcGen; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
): int =
  var g = TSrcLen.init(g)
  gsons(g, n, start, theEnd, flags)
  g.lineLen

proc lsons(
    g: var TSrcLen; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
): int =
  0

proc lcomma(
    g: TSrcGen;
    n: PNode;
    start: int = 0;
    theEnd: int = -1;
    separator = tkComma;
    indentNL: int = IndentWidth;
    flags: ListFlags = {};
    subFlags: SubFlags = {};
): int =
  var g = TSrcLen.init(g)
  gcomma(g, n, start, theEnd, separator, indentNL, flags, subFlags)
  g.lineLen

proc lcomma(
    g: TSrcLen;
    n: PNode;
    start: int = 0;
    theEnd: int = -1;
    separator = tkComma;
    indentNL: int = IndentWidth;
    flags: ListFlags = {};
    subFlags: SubFlags = {};
): int =
  0

proc llist(
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
): int =
  var g = TSrcLen.init(g)
  glist(g, n, brOpen, separator, extra, start, theEnd, indentNL, flags, subFlags)
  g.lineLen + extra

proc llist(
    g: var TSrcLen;
    n: PNode;
    brOpen = tkParLe;
    separator = tkComma;
    extra = 0;
    start = 0;
    theEnd = -1;
    indentNL = IndentWidth;
    flags: ListFlags = {};
    subFlags: SubFlags = {};
): int =
  0

proc nlsub(g: TOutput; n: PNode; flags: SubFlags = {}): int =
  ## How many characters until the next early line break
  case n.kind
  of nkPar, nkCurly, nkBracket, nkTableConstr, nkStmtListExpr:
    1
  of nkPragma, nkPragmaExpr:
    2
  else:
    lsub(g, n, flags)

proc fits(g: TSrcLen; x: int): bool =
  # Line lengths are computed assuming no extra line breaks
  true

proc fits(g: TSrcGen; x: int): bool =
  result = x <= MaxLineLen

proc overflows(g: TOutput; x: int): bool =
  not fits(g, g.lineLen + x)

proc putWithSpace(g: var TOutput; kind: TokType; s: string) =
  put(g, kind, s)
  put(g, tkSpaces, Space)

proc gextras(g: var TSrcGen; toks: openArray[Token]; indented, firstSticky: bool) =
  if toks.len == 0:
    return

  let long = toks.len > 1 or overflows(g, len($toks[0]))
  if indented:
    optIndent(g)

  if long and not firstSticky:
    optNL(g)

  for tok in toks:
    g.pendingNewline = g.pendingNewline or tok.prevLine + 1 < tok.line
    optSpace(g)
    put(g, tok.tokType, $tok)
    optNL(g)

  if indented:
    dedent(g)

proc gprefixes(g: var TSrcGen; n: PNode) =
  if n.prefix.len > 0:
    # Putting prefixes on the same line as the node helps comment stability
    g.optNL()
  gextras(g, n.prefix, false, false)

proc gmids(g: var TSrcGen; n: PNode; indented = false) =
  gextras(g, n.mid, indented, false)

proc gpostfixes(g: var TSrcGen; n: PNode; firstSticky = false) =
  # Postfixes are indented to increase chances that they stick
  # with the same node on re-parse since in most cases, we split
  # comments between prefix and postfix indent being greater than the
  # non-comment token that follows

  # Suspent kind-specific blanks while rendering postfixes
  let blank = g.pendingNewline
  g.pendingNewline = false
  gextras(g, n.postfix, true, firstSticky)
  g.pendingNewline = blank

proc gprefixes(g: var TSrcLen; n: PNode) =
  discard

proc gmids(g: var TSrcLen; n: PNode; indented = false) =
  discard

proc gpostfixes(g: var TSrcLen; n: PNode; firstSticky = false) =
  discard

proc eqIdent(n: PNode; s: string): bool =
  n.kind == nkIdent and n.ident != nil and n.ident.s.cmpIgnoreStyle(s) == 0

proc isOperatorFirst(g: TOutput; n: PNode): bool =
  case n.kind
  of nkInfix:
    isOperatorFirst(g, n[1])
  of nkIdent:
    n.ident.s.len > 0 and n.ident.s[0] in OpChars
  of nkCharLit..nkUInt64Lit, nkFloatLit..nkFloat128Lit:
    let lit = atom(g, n)
    lit.len > 0 and lit[0] in OpChars
  of nkStrLit..nkTripleStrLit, nkCommentStmt:
    false
  of nkPar:
    false
  else:
    isOperatorFirst(g, n[0])

proc gcomma(
    g: var TOutput;
    n: PNode;
    start: int = 0;
    theEnd: int = -1;
    separator = tkComma;
    indentNL: int = IndentWidth;
    flags: ListFlags = {};
    subFlags: SubFlags = {};
) =
  var indented = false
  defer:
    if indented:
      g.dedent(indentNL)

  let
    (start, count) =
      if lfFirstSticky in flags:
        let count = n.len + theEnd - start + 1
        if count == 0:
          return

        # The first item must be rendered without newlines, so we start with that
        gsub(g, n[start], flags = subFlags + {sfSkipPostfix})

        if count > 1:
          putWithSpace(g, separator, $separator)

        # Postfixes after separator!
        gpostfixes(g, n[start], lfFirstCommentSticky in flags)

        # If we can't fit everything on the current line, start over at a fresh one
        if lfFirstAlone in flags and
            overflows(
              g,
              lcomma(
                g,
                n,
                start + 1,
                theEnd,
                separator,
                indentNL = 0,
                flags - {lfFirstSticky, lfFirstAlone},
                subFlags,
              ),
            ) or n[start].postfix.len > 0:
          indented = true
          g.indentNL(indentNL)

        (start + 1, count - 1)
      else:
        (start, n.len + theEnd - start + 1)

  if count == 0:
    return

  if not indented:
    indented = true
    g.indent += indentNL

  # If a full, comma-separate list fits on one line, go for it. If not, we put
  # each element on its own line unless it's a list of trivial things (so as to
  # avoid wasting significant vertical space on lists of numbers and the like)
  let
    onePerLine =
      if fits(
        g,
        g.lineLen +
          lcomma(
            g,
            n,
            start,
            theEnd,
            separator,
            indentNL,
            flags - {lfFirstSticky, lfFirstAlone},
            subFlags,
          ),
      ):
        false
      else:
        count > 1 and
          anyIt(n.sons[start..n.len + theEnd], not isSimple(it, n.kind == nkIdentDefs))

  for i in start..n.len + theEnd:
    let c = i < n.len + theEnd
    if onePerLine or overflows(g, lsub(g, n[i]) + ord(c)):
      optNL(g)

    let oldLen = g.tokens.len

    gsub(g, n[i], flags = subFlags + {sfSkipPostfix})

    if c:
      if g.tokens.len > oldLen:
        if lfSkipPushComma notin flags or not eqIdent(n[i], "push"):
          putWithSpace(g, separator, $separator)
        else:
          optSpace(g)

    gpostfixes(g, n[i], lfFirstCommentSticky in flags)

  if lfSepAtEnd in flags or onePerLine and lfLongSepAtEnd in flags:
    put(g, separator, $separator)

proc gsons(
    g: var TOutput; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
) =
  for i in start..n.len + theEnd:
    gsub(g, n[i], flags)

proc gsonsNL(
    g: var TOutput; n: PNode; start: int = 0; theEnd: int = -1; flags: SubFlags = {}
) =
  for i in start..n.len + theEnd:
    gsub(g, n[i], flags)
    g.optNL()

proc glist(
    g: var TOutput;
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
  let
    brClose = brCloseOf(brOpen)
    len =
      llist(g, n, brOpen, separator, extra, start, theEnd, indentNL, flags, subFlags)
    withNL = n.len + theEnd >= start and (overflows(g, len) or n.mid.len > 1)

  if brClose != tkInvalid:
    # TODO stack all opening brackets on one line if there are many
    put(g, brOpen, $brOpen)

  optIndent(g, indentNL)
  if n.mid.len == 1:
    # Rendering the mid before the rest of the list helps comment stability
    gmids(g, n)

  if withNL:
    g.optNL()

  if n.mid.len != 1:
    gmids(g, n)

  gcomma(
    g, n, start, theEnd, separator, indentNL = 0, flags = flags, subFlags = subFlags
  )

  dedent(g, indentNL)

  # If comments caused a newline, make sure we end with a newline as well
  if withNL or n.mid.len > 0:
    g.optNL()

  if brClose != tkInvalid:
    put(g, brClose, $brClose)

proc gsection(g: var TOutput; n: PNode; kind: TokType; k: string) =
  if n.len == 0:
    return
  # empty var sections are possible
  putWithSpace(g, kind, k)

  let
    complex =
      n.len > 1 or n.mid.len > 0 or n[0].prefix.len > 0 or overflows(g, lsub(g, n[0]))
  if complex:
    gmids(g, n, true)

    indentNL(g)
    for i in 0..<n.len:
      if i > 0:
        optNL(g, n[i - 1], n[i])
      else:
        optNL(g)
      gsub(g, n[i])

    dedent(g)
  else:
    gsub(g, n[0])

proc gstmts(g: var TOutput; n: PNode; flags: SubFlags = {}; doIndent = true) =
  if n.kind == nkEmpty:
    gprefixes(g, n)
    if sfSkipPostfix notin flags:
      gpostfixes(g, n)

    return

  let
    needsPar = n.kind == nkStmtListExpr
      # `nkStmtListExpr` is generated from an `nkPar` in the parser in certain
      # cases - see `parsePar` which calls `semiStmtList` - in particular, it
      # could be rendered with `;` instead of newline

    trivial = needsPar and not g.overflows(lsons(g, n))

  if needsPar:
    put(g, tkParLe, $tkParLe)
    # Sometimes a semi-colon is needed to get the right parsing
    if not trivial and
        not (
          n.len > 0 and
          n[0].kind in {
            nkIfStmt, nkWhenStmt, nkWhileStmt, nkDiscardStmt, nkTryStmt, nkBlockStmt,
            nkLetSection, nkVarSection, nkConstSection, nkCaseStmt
          }
        ):
      put(g, tkSemiColon, $tkSemiColon)

  if (doIndent or needsPar) and not trivial:
    indentNL(g)

  let
    flags =
      if doIndent or needsPar:
        flags + {sfNoIndent}
      else:
        flags

  # The original version tried to normalize newlines but this turned out to be
  # difficult to do whole-sale - we will thus retain some of the original
  # new-lines, normalizing only where a clear rule can be established
  if n.kind in {nkStmtList, nkStmtListExpr, nkStmtListType}:
    gprefixes(g, n)
    var j = 0
    for i in 0..<n.len:
      # This groups sections by their kind, giving a bit of air between "parts"
      # of code - we don't do it before control flow because there, the code
      # before the control flow is often part of settting it up
      # TODO if the previous statement was complex (lots of lines / indent) we should
      #      probably add a newline even before control flow
      if n[i].kind == nkEmpty:
        continue

      if j > 0:
        if trivial:
          putWithSpace(g, tkSemiColon, $tkSemiColon)
        else:
          optNL(g, n[i - 1], n[i])
      # else:
      #   optNL(g)
      if n[i].kind in {nkStmtList, nkStmtListExpr, nkStmtListType}:
        gstmts(g, n[i], flags = flags, doIndent = false)
      else:
        gsub(g, n[i], flags = {sfSkipDo})

      j += 1

    if sfSkipPostfix notin flags:
      gpostfixes(g, n)
  else:
    gsub(g, n, flags)

  if (doIndent or needsPar) and not trivial:
    dedent(g)

  if not trivial:
    optNL(g)
  if needsPar:
    # No EOL after `)` or things will get misparsed!
    put(g, tkParRi, $tkParRi)

proc gcolcoms(g: var TOutput; n, stmts: PNode; useSub = false) =
  putWithSpace(g, tkColon, ":")
  if stmts.kind == nkStmtList and stmts.len == 0 and n.mid.len > 0:
    g.optNL() # doc-comment-only
  gmids(g, n, true)
  if useSub:
    gsub(g, stmts)
  else:
    gstmts(g, stmts) # Adds a newline after each child

proc gcond(g: var TOutput; n: PNode; flags: SubFlags = {}) =
  gsub(g, n, flags)

proc gif(g: var TOutput; n: PNode; flags: SubFlags) =
  gprefixes(g, n[0])
  let
    sublen = lsub(g, n[0][0]) + lsub(g, n[0][1]) + lsons(g, n, 1) + len(":")
    simple = not overflows(g, sublen)

  gcond(g, n[0][0], {sfLongIndent})
  gcolcoms(g, n[0], n[0][1], true)

  if sfSkipPostfix notin flags:
    gpostfixes(g, n[0])

  if not simple:
    optNL(g)
    gsonsNL(g, n, 1)
  else:
    gsons(g, n, 1)

proc gwhile(g: var TOutput; n: PNode) =
  putWithSpace(g, tkWhile, "while")
  gcond(g, n[0], {sfLongIndent})
  gcolcoms(g, n, n[1])

proc gpattern(g: var TOutput; n: PNode) =
  put(g, tkCurlyLe, "{")
  gmids(g, n, true)
  gstmts(g, n)
  put(g, tkCurlyRi, "}")

proc gpragmaBlock(g: var TOutput; n: PNode) =
  gsub(g, n[0])
  gcolcoms(g, n, n[1])

proc gtry(g: var TOutput; n: PNode) =
  put(g, tkTry, "try")
  gcolcoms(g, n, n[0])
  gsons(g, n, start = 1)

proc gfor(g: var TOutput; n: PNode) =
  putWithSpace(g, tkFor, "for")
  gcomma(g, n, start = 0, theEnd = -3)
  optSpace(g)
  putWithSpace(g, tkIn, "in")
  gsub(g, n[^2], flags = {sfLongIndent})
  gcolcoms(g, n, n[^1])

proc gcase(g: var TOutput; n: PNode) =
  if n.len == 0:
    return

  putWithSpace(g, tkCase, "case")
  gcond(g, n[0])
  gmids(g, n)
  optNL(g)
  gsons(g, n, start = 1)

proc gproc(g: var TOutput; n: PNode) =
  gsub(g, n[namePos])

  if n[patternPos].kind != nkEmpty:
    gpattern(g, n[patternPos])

  # If there is no body, we don't need to indent the parameters as much
  let
    flags =
      if n[bodyPos].kind != nkEmpty:
        {sfLongIndent}
      else:
        {}

  gsub(g, n[genericParamsPos], flags)
  gsub(g, n[paramsPos], flags)
  gsub(g, n[pragmasPos], flags)
  if n[bodyPos].kind != nkEmpty:
    optSpace(g)
    putWithSpace(g, tkEquals, "=")
    if n[bodyPos].kind == nkStmtList and n[bodyPos].len == 0:
      # lonely mid comment needs to sit on a new line
      withIndent(g):
        g.optNL()
        gmids(g, n)
    else:
      gmids(g, n, true)

      gstmts(g, n[bodyPos], flags)
  else:
    withIndent(g):
      gmids(g, n)

proc gTypeClassTy(g: var TOutput; n: PNode) =
  putWithSpace(g, tkConcept, "concept")
  g.inConcept += 1
  gcomma(g, n[0]) # arglist
  gsub(g, n[1]) # pragmas
  gsub(g, n[2]) # of
  gmids(g, n)
  indentNL(g)
  gstmts(g, n[3])
  dedent(g)

  g.inConcept -= 1

proc gblock(g: var TOutput; n: PNode) =
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

  gcolcoms(g, n, n[1])

proc gstaticStmt(g: var TOutput; n: PNode) =
  put(g, tkStatic, "static")
  gcolcoms(g, n, n[0])

proc gasm(g: var TOutput; n: PNode) =
  putWithSpace(g, tkAsm, "asm")
  gsub(g, n[0])
  if n.len > 1:
    gsub(g, n[1])

proc gident(g: var TOutput; n: PNode) =
  var t: TokType
  var s = atom(g, n)
  if s.len > 0 and s[0] in phlexer.SymChars:
    if n.kind == nkIdent:
      if (n.ident.id < ord(tokKeywordLow) - ord(tkSymbol)) or (
        n.ident.id > ord(tokKeywordHigh) - ord(tkSymbol)
      ):
        t = tkSymbol
      else:
        t = TokType(n.ident.id + ord(tkSymbol))
    else:
      t = tkSymbol
  else:
    t = tkOpr

  put(g, t, s)

proc doParamsAux(g: var TOutput; params: PNode) =
  let
    retLen =
      if params.len > 0 and params[0].kind != nkEmpty:
        lsub(g, params[0]) + len(" -> ")
      else:
        0

  if params.len > 0:
    # We must always output () or we get a significantly different AST (!)
    glist(g, params, tkParLe, tkSemiColon, extra = retLen, start = 1)

  if params.len > 0 and params[0].kind != nkEmpty:
    optSpace(g)
    putWithSpace(g, tkOpr, "->")
    gsub(g, params[0])

proc gsubOptNL(
    g: var TOutput;
    n: PNode;
    indentNL = IndentWidth;
    flags: SubFlags = {};
    strict = false;
) =
  # Output n on the same line if it fits, else continue on next - indentation is
  # always set up in case a comment linebreaks the statement

  if hasIndent(n) and n.kind in subOptSkipNL:
    gsub(g, n, flags = flags)
  else:
    let
      nl =
        overflows(
          g,
          if strict:
            lsub(g, n)
          else:
            nlsub(g, n)
          ,
        )
    withIndent(g, indentNL):
      if nl:
        optNL(g)

      gsub(g, n, flags = flags)

proc accentedName(g: var TOutput; n: PNode) =
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

proc infixArgument(g: var TOutput; n: PNode; i: int; flags: SubFlags) =
  if i < 1 or i > 2:
    return

  let needsParenthesis = infixHasParens(n, i)

  if needsParenthesis:
    put(g, tkParLe, "(")

  gcond(g, n[i], flags)
  if needsParenthesis:
    put(g, tkParRi, ")")

const
  postExprBlocks = {
    nkStmtList, nkStmtListExpr, nkOfBranch, nkElifBranch, nkElse, nkExceptBranch,
    nkFinally, nkDo
  }

proc postStatements(
    g: var TOutput; n: PNode; start: int; skipDo: bool; skipColon = false
) =
  # Sometimes, `do` can be skipped but it is not entirely clear when - this
  # feature rests on experiments with large codebases but should be researched
  # better
  var i = start
  if n[i].kind in {nkStmtList, nkStmtListExpr}:
    if skipDo:
      if not skipColon:
        put(g, tkColon, ":")
    else:
      optSpace(g)
      put(g, tkDo, "do")
      put(g, tkColon, ":")

  gsub(g, n[i])

  i.inc
  for j in i..<n.len:
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

proc gsub(g: var TOutput; n: PNode; flags: SubFlags) =
  if isNil(n):
    return

  if n.kind in {nkStmtList, nkStmtListExpr, nkStmtListType}:
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

      postStatements(g, n, i, sfSkipDo in flags)
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
    if n.len > 1 and n.lastSon.kind in postExprBlocks:
      var i = 2
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc
      postStatements(g, n, i, sfSkipDo in flags, n[i].kind == nkStmtListExpr)
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
    putWithSpace(g, tkStatic, "static")
    gsub(g, n[0])
  of nkBracketExpr:
    gcond(g, n[0])
    glist(
      g,
      n,
      tkBracketLe,
      start = 1,
      indentNL = flagIndent(flags),
      flags = {lfLongSepAtEnd},
    )
  of nkCurlyExpr:
    gcond(g, n[0])
    glist(
      g, n, tkCurlyLe, start = 1, indentNL = flagIndent(flags), flags = {lfLongSepAtEnd}
    )
  of nkPragmaExpr:
    gsub(g, n[0], flags)
    gsub(g, n[1], flags)
  of nkCommand:
    accentedName(g, n[0])
    optSpace(g)

    if n.len > 1 and n.lastSon.kind in postExprBlocks:
      var i = 1
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc

      if i > 1:
        gcomma(
          g,
          n,
          1,
          i - 1 - n.len,
          indentNL = IndentWidth,
          flags = {lfFirstSticky, lfFirstAlone},
        )

      # when parsing nkCommand, the compiler inserts `nkCall` to arguments if
      # ":" is present so it looks like we can skip the `do` here :/ this needs
      # deeper investigation - see also `nkPar` which sometimes removes the
      # parenthesis from the AST
      postStatements(g, n, i, sfSkipDo in flags, n[i].kind == nkStmtListExpr)
    else:
      # The first argument must not be line-broken, or command syntax breaks!
      if n.len > 1:
        gcomma(g, n, 1, indentNL = IndentWidth, flags = {lfFirstSticky, lfFirstAlone})
  of nkExprEqExpr, nkAsgn, nkFastAsgn:
    gsub(g, n[0])
    optSpace(g)
    putWithSpace(g, tkEquals, "=")
    gmids(g, n, true)
    gsubOptNL(g, n[1], flags = {sfSkipDo})
  of nkPar, nkClosure:
    glist(g, n, tkParLe, subflags = {sfNoIndent})
  of nkTupleConstr:
    let
      flags =
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
        optNL(g)
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
    optSpace(g)
    putWithSpace(g, tkEquals, "=")
    gmids(g, n, true)
    gsubOptNL(g, n[bodyPos], strict = true)
  of nkDo:
    optSpace(g)
    put(g, tkDo, $tkDo)
    if paramsPos < n.len:
      doParamsAux(g, n[paramsPos])

    gsub(g, n[pragmasPos])
    gcolcoms(g, n, n[bodyPos])
  of nkIdentDefs:
    gcomma(
      g, n, theEnd = -3, indentNL = 2, flags = {lfFirstSticky, lfFirstCommentSticky}
    )

    if n.len >= 2 and n[^2].kind != nkEmpty:
      putWithSpace(g, tkColon, ":")
      gsubOptNL(g, n[^2])

    if n.len >= 1 and n[^1].kind != nkEmpty:
      optSpace(g)
      putWithSpace(g, tkEquals, "=")
      gsubOptNL(g, n[^1], flags = {sfSkipDo}, strict = true)
  of nkConstDef:
    gcomma(g, n, theEnd = -3, indentNL = 0, flags = {lfFirstSticky})
    if n.len >= 2 and n[^2].kind != nkEmpty:
      putWithSpace(g, tkColon, ":")
      gsubOptNL(g, n[^2])

    if n.len >= 1 and n[^1].kind != nkEmpty:
      optSpace(g)
      putWithSpace(g, tkEquals, "=")
      gsubOptNL(g, n[^1], flags = {sfSkipDo}, strict = true)
  of nkVarTuple:
    if n[^1].kind == nkEmpty:
      glist(g, n, tkParLe, theEnd = -2)
    else:
      glist(g, n, tkParLe, extra = len(" = "), theEnd = -3)
      optSpace(g)
      putWithSpace(g, tkEquals, "=")
      gsubOptNL(g, n[^1], flags = {sfSkipDo})
  of nkExprColonExpr:
    gsub(g, n[0])
    putWithSpace(g, tkColon, ":")
    gsubOptNL(g, n[1])
  of nkInfix:
    if n.len < 3:
      put(g, tkOpr, "Too few children for nkInfix")

      return

    infixArgument(g, n, 1, flags = flags)

    let
      spaces =
        not (
          (g.inImportLike > 0 and eqIdent(n[0], "/")) or (
            (eqIdent(n[0], "..") or eqIdent(n[0], "..<") or eqIdent(n[0], "..^")) and
            not isOperatorFirst(g, n[2])
          )
        )

    if spaces:
      optSpace(g)

    gsub(g, n[0], flags = flags) # binary operator

    let
      overflows =
        n.len == 3 and overflows(g, nlsub(g, n[2])) and not infixHasParens(n, 2)
      indent = overflows and sfNoIndent notin flags and not hasIndent(n[2])
      wid = flagIndent(flags)
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
      optSpace(g)

    infixArgument(g, n, 2, flags = flags)
    if indent:
      dedent(g, wid)

    if n.len > 3 and n.lastSon.kind in postExprBlocks:
      var i = 3
      while i < n.len and n[i].kind notin postExprBlocks:
        i.inc

      postStatements(g, n, i, sfSkipDo in flags)
  of nkPrefix:
    gsub(g, n[0])
    if n.len > 1:
      let
        opr =
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
        optSpace(g)

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

      postStatements(g, n, i, sfSkipDo in flags)
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
    for i in 0..<n.len:
      proc isAlpha(n: PNode): bool =
        if n.kind in {nkIdent, nkSym}:
          let tmp = n.getPIdent.s

          result = tmp.len > 0 and tmp[0] in {'a'..'z', 'A'..'Z'}

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
        optSpace(g)

      gsub(g, n[i])

    put(g, tkAccent, "`")
  of nkIfExpr:
    putWithSpace(g, tkIf, "if")
    gif(g, n, flags)
  of nkElifExpr:
    optSpace(g)
    putWithSpace(g, tkElif, "elif")
    gcond(g, n[0])
    gcolcoms(g, n, n[1], true)
  of nkElseExpr:
    optSpace(g)
    put(g, tkElse, "else")
    gcolcoms(g, n, n[0], true)
  of nkTypeOfExpr:
    if g.inConcept > 0:
      putWithSpace(g, tkType, "type")
      gsub(g, n[0])
    else:
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

    optSpace(g)
    if n.len > 2 and n[2].kind != nkEmpty:
      putWithSpace(g, tkEquals, "=")
      if n[2].kind in {nkObjectTy, nkEnumTy, nkRefTy}:
        gsub(g, n[2])
      else:
        gsubOptNL(g, n[2], strict = true)

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
    for i in 0..<n.len:
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
    optSpace(g)
    putWithSpace(g, tkEquals, "=")
    gsub(g, n[1])
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
        gsub(g, n[0], flags = {sfSkipDo})
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
    put(g, tkDiscard, "discard")
    if n[0].kind != nkEmpty:
      withIndent(g):
        optSpace(g)
        gmids(g, n)
        gsub(g, n[0], flags = {sfSkipDo})
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

      optSpace(g)

      glist(g, n, tkCurlyDotLe, indentNL = flagIndent(flags), flags = {lfSkipPushComma})

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
    optSpace(g)
    putWithSpace(g, tkExcept, "except")
    glist(g, n, brOpen = tkInvalid, start = 1)

    g.inImportLike -= 1
  of nkFromStmt:
    g.inImportLike += 1

    putWithSpace(g, tkFrom, "from")
    gsub(g, n[0])
    optSpace(g)
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
    gcolcoms(g, n, n[^1], true)
  of nkImportAs:
    gsub(g, n[0])
    optSpace(g)
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
    gcolcoms(g, n, n[1])
  of nkElse:
    optNL(g)
    put(g, tkElse, "else")
    gcolcoms(g, n, n[0])
  of nkFinally, nkDefer:
    optNL(g)
    if n.kind == nkFinally:
      put(g, tkFinally, "finally")
    else:
      put(g, tkDefer, "defer")

    gcolcoms(g, n, n[0])
  of nkExceptBranch:
    optNL(g)
    if n.len != 1:
      putWithSpace(g, tkExcept, "except")
    else:
      put(g, tkExcept, "except")

    gcomma(g, n, 0, -2, indentNL = longIndentWid)
    gcolcoms(g, n, n[^1])
  of nkGenericParams:
    glist(g, n, tkBracketLe, tkSemiColon, indentNL = flagIndent(flags))
  of nkFormalParams:
    # Need to add empty parens here, or nkProcTy parsing becomes non-equal -
    # see hasSignature - it would be nicer to remove them when unncessary
    if n.len >= 1:
      let
        retExtra =
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
        indentNL = flagIndent(flags),
        flags = {lfLongSepAtEnd},
      )

    if n.len > 0 and n[0].kind != nkEmpty:
      putWithSpace(g, tkColon, ":")
      gsub(g, n[0], flags)
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
