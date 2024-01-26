#           nph
#        (c) Copyright 2023 Jacek Sieka
## Compare two AST's for semantic equivalence - aka undo whitespace bugs in the
## Nim parser / grammar
import "$nim"/compiler/[ast, llstream, parser, idents, options, pathutils], std/sequtils

from std/math import isNaN

type
  Equivalence* = enum
    Same
    ParseError
    Different

  Outcome* = object
    case kind*: Equivalence
    of Same, ParseError:
      discard
    of Different:
      a*, b*: PNode

# TODO https://github.com/nim-lang/Nim/pull/23088
proc parseAll(p: var Parser): PNode =
  ## Parses the rest of the input stream held by the parser into a PNode.
  result = newNodeP(nkStmtList, p)
  while true:
    let nextStmt = p.parseTopLevelStmt()
    if nextStmt.kind == nkEmpty:
      break
    result &= nextStmt
  # setEndInfo()

proc parseString2(
    s: string,
    cache: IdentCache,
    config: ConfigRef,
    filename: string = "",
    line: int = 0,
    printTokens = false,
): PNode =
  ## Parses a string into an AST, returning the top node.
  ## `filename` and `line`, although optional, provide info so that the
  ## compiler can generate correct error messages referring to the original
  ## source.
  var stream = llStreamOpen(s)
  stream.lineOffset = line

  var p: Parser

  openParser(p, AbsoluteFile filename, stream, cache, config)
  result = p.parseAll
  closeParser(p)

proc similarKinds(ak, bk: TNodeKind): bool =
  ak == bk or (ak in {nkElseExpr, nkElse} and bk in {nkElseExpr, nkElse}) or (
    ak in {nkElifExpr, nkElifBranch} and bk in {nkElifExpr, nkElifBranch}
  )

proc equivalent*(a, b: PNode): Outcome =
  if not similarKinds(a.kind, b.kind):
    # Semantically equivalent ways of representing the same tree - difference
    # lies in how much whitespace we introduce (unfortunately)
    if b.kind in {nkFormalParams, nkRecList, nkStmtList, nkStmtListExpr} and
        b.sons.len == 1:
      return equivalent(a, b.sons[0])

    if a.kind in {nkFormalParams, nkRecList, nkStmtList, nkStmtListExpr} and
        a.sons.len == 1:
      return equivalent(a.sons[0], b)

    if b.kind in {nkFormalParams, nkRecList, nkStmtList, nkStmtListExpr} and
        b.sons.len == 0 and a.kind == nkEmpty:
      return Outcome(kind: Same)

    if a.kind in {nkFormalParams, nkRecList, nkStmtList, nkStmtListExpr} and
        a.sons.len == 0 and b.kind == nkEmpty:
      return Outcome(kind: Same)

    if a.kind == nkPrefix and a.len == 2 and a[0].kind == nkIdent and a[0].ident.s == "-" and
        b.kind == a[1].kind:
      # `- 1` is transformed to `-1` which semantically is not exactly the same but close enough
      # TODO the positive and negative ranges of integers are not the same - is this a problem?
      #      what about more complex expressions?
      return Outcome(kind: Same)
    # runnableExamples: static: ... turns into a staticStmt when broken up in
    # lines (!)
    if a.kind == nkCall and b.kind == nkStaticStmt and a.sons.len > 1 and
        a.sons[0].kind == nkIdent and a.sons[0].ident.s == "static":
      return equivalent(a.sons[1], b.sons[0])

    return Outcome(kind: Different, a: a, b: b)

  let eq =
    case a.kind
    of nkCharLit .. nkUInt64Lit:
      a.intVal == b.intVal
    of nkFloatLit .. nkFloat128Lit:
      (isNaN(a.floatVal) and isNaN(b.floatVal)) or a.floatVal == b.floatVal
    of nkStrLit .. nkTripleStrLit:
      a.strVal == b.strVal
    of nkSym:
      raiseAssert "Shouldn't eixst in parser"
    of nkIdent:
      a.ident.s == b.ident.s
    else:
      let skipped =
        if a.kind == nkStmtListExpr:
          # When inserting a `;`, we might get some extra empty statements (?)
          {nkEmpty, nkCommentStmt}
        else:
          {nkCommentStmt}
      # TODO don't break comments!
      let
        af = a.sons.filterIt(it.kind notin skipped)
        bf = b.sons.filterIt(it.kind notin skipped)

      if af.len() != bf.len():
        false
      else:
        for (aa, bb) in zip(af, bf):
          let eq = equivalent(aa, bb)
          if eq.kind == Different:
            return eq

        true

  if not eq:
    Outcome(kind: Different, a: a, b: b)
  else:
    Outcome(kind: Same)

proc makeConfigRef(): ConfigRef =
  let conf = newConfigRef()
  conf.errorMax = int.high
  conf

proc equivalent*(a, afile, b, bfile: string): Outcome =
  let
    conf = makeConfigRef()
    aa = parseString2(a, newIdentCache(), conf, afile)
    bb = parseString2(b, newIdentCache(), conf, bfile)
  if conf.errorCounter > 0:
    Outcome(kind: ParseError)
  else:
    equivalent(aa, bb)
