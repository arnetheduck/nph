#
#
#           nph
#        (c) Copyright 2023 Jacek Sieka
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module implements the parser of the standard Nim syntax.
# The parser strictly reflects the grammar ("doc/grammar.txt"); however
# it uses several helper routines to keep the parser small. A special
# efficient algorithm is used for the precedence levels. The parser here can
# be seen as a refinement of the grammar, as it specifies how the AST is built
# from the grammar and how comments belong to the AST.
#
# nph version:
# * analyse comment structure in parser instead of lexer
# * attach comments to nodes as prefix/mid/postfix to anchor them to the most
#   plausible node that they belong to:
#   - prefix for comments that precede a construct
#   - mid for comments that appear in the middle of complex syntax nodes, ie
#     between `:` and statements of an `if`
#   - postfix for comments at a greater column / level than the indent level or
#     starting column of the current node.
#
# The comment strategy rouhgly corresponds to the spirit of the indent rules of
# Nim, ie that the field of an object belong to the object as long as they're
# indented more than what the indent level was at the start of the object.

import "$nim"/compiler/[llstream, idents, pathutils], std/[strutils, sequtils]
import "."/[phast, phlexer, phlineinfos, phmsgs, phoptions]
when defined(nimPreviewSlimSystem):
  import std/assertions

type
  Parser* = object
    # A Parser object represents a file that
    # is being parsed
    currInd: int # current indentation level
    firstTok: bool # Has the first token been read?
    hasProgress: bool # some while loop requires progress ensurance
    lex*: Lexer # The lexer that is used for parsing
    tok*: Token # The current token
    lineStartPrevious*: int
    lineNumberPrevious*: int
    bufposPrevious*: int
    inPragma*: int # Pragma level
    inSemiStmtList*: int
    emptyNode: PNode
    skipped*: seq[Token]
    endLine: int

  SymbolMode = enum
    smNormal
    smAllowNil
    smAfterDot

  PrimaryMode = enum
    pmNormal
    pmTypeDesc
    pmTypeDef
    pmTrySimple

  CommentLoc = enum
    clPrefix
    clMid
    clPostfix

proc parseTry(p: var Parser, isExpr: bool): PNode
proc parseCase(p: var Parser): PNode
proc parseStmtPragma(p: var Parser): PNode
proc parsePragma(p: var Parser): PNode
proc postExprBlocks(p: var Parser, x: PNode): PNode
proc parseBlock(p: var Parser): PNode
proc primary(p: var Parser, mode: PrimaryMode): PNode
proc simpleExprAux(p: var Parser, limit: int, mode: PrimaryMode): PNode

proc getTok(p: var Parser) =
  ## Get the next token from the parser's lexer, and store it in the parser's
  ## `tok` member.
  ##
  ## Comments are placed in the "skipped" member so that logic looking for
  ## specific node types can find what it's looking for.
  p.lineNumberPrevious = p.lex.lineNumber
  p.lineStartPrevious = p.lex.lineStart
  p.bufposPrevious = p.lex.bufpos
  rawGetTok(p.lex, p.tok)

  while p.tok.tokType == tkComment:
    p.skipped.add p.tok
    rawGetTok(p.lex, p.tok)

  p.hasProgress = true

proc openParser*(
    p: var Parser,
    fileIdx: FileIndex,
    inputStream: PLLStream,
    cache: IdentCache,
    config: ConfigRef,
    printTokens: bool,
) =
  ## Open a parser, using the given arguments to set up its internal state.
  ##
  reset(p.tok)
  openLexer(p.lex, fileIdx, inputStream, cache, config, printTokens)
  getTok(p) # read the first token

  p.firstTok = true
  p.emptyNode = newNode(nkEmpty)

proc openParser*(
    p: var Parser,
    filename: AbsoluteFile,
    inputStream: PLLStream,
    cache: IdentCache,
    config: ConfigRef,
    printTokens: bool,
) =
  openParser(p, fileInfoIdx(config, filename), inputStream, cache, config, printTokens)

proc closeParser(p: var Parser) =
  ## Close a parser, freeing up its resources.
  closeLexer(p.lex)

proc parMessage(p: Parser, msg: TMsgKind, arg = "") =
  ## Produce and emit the parser message `arg` to output.
  lexMessageTok(p.lex, msg, p.tok, arg)

proc parMessage(p: Parser, msg: string, tok: Token) =
  ## Produce and emit a parser message to output about the token `tok`
  parMessage(p, errGenerated, msg % prettyTok(tok))

proc parMessage(p: Parser, arg: string) =
  ## Produce and emit the parser message `arg` to output.
  lexMessageTok(p.lex, errGenerated, p.tok, arg)

template withInd(p, indent, body: untyped) =
  let oldInd = p.currInd
  p.currInd = indent
  body
  p.currInd = oldInd

template withInd(p, body: untyped) =
  withInd(p, p.tok.indent, body)

template realInd(p): bool =
  p.tok.indent > p.currInd

template sameInd(p): bool =
  p.tok.indent == p.currInd

template sameOrNoInd(p): bool =
  p.tok.indent == p.currInd or p.tok.indent < 0

proc validInd(p: var Parser): bool {.inline.} =
  result = p.tok.indent < 0 or p.tok.indent > p.currInd

proc addComment(node: PNode, tok: Token, commentLoc: CommentLoc) =
  case commentLoc
  of clPrefix:
    node.prefix.add tok
  of clMid:
    node.mid.add tok
  of clPostfix:
    node.postfix.add tok

const
  errInvalidIndentation = "invalid indentation $1"
  errIdentifierExpected = "identifier expected, but got '$1'"
  errExprExpected = "expression expected, but found '$1'"

proc skipInd(p: var Parser) =
  if p.tok.indent >= 0:
    if not realInd(p):
      parMessage(p, errInvalidIndentation % "skipInd")

proc optPar(p: var Parser) =
  if p.tok.indent >= 0:
    if p.tok.indent < p.currInd:
      parMessage(p, errInvalidIndentation % "optPar")

proc optInd(p: var Parser, n: PNode) =
  # see getTok for comment lookahead handling
  # skipComment(p, n, commentLoc)
  skipInd(p)

proc getTokNoInd(p: var Parser) =
  getTok(p)
  if p.tok.indent >= 0:
    parMessage(p, errInvalidIndentation % "getTokNoInd")

proc expectIdent(p: Parser) =
  if p.tok.tokType != tkSymbol:
    lexMessage(p.lex, errGenerated, errIdentifierExpected % prettyTok(p.tok))

proc eat(p: var Parser, tokType: TokType) =
  ## Move the parser to the next token if the current token is of type
  ## `tokType`, otherwise error.
  if p.tok.tokType == tokType:
    getTok(p)
  else:
    lexMessage(
      p.lex,
      errGenerated,
      "expected: '" & $tokType & "', but got: '" & prettyTok(p.tok) & "'",
    )

proc parLineInfo(p: Parser): TLineInfo =
  ## Retrieve the line information associated with the parser's current state.
  result = getLineInfo(p.lex, p.tok)

template setEndInfo(node: PNode) =
  # End info is used to compute original line breaks
  # TODO this endinfo is not quite correct - we only need to track this for
  #      for statement lists, which is the only place where we retain whitespace
  node.endInfo = TLineInfo(
    fileIndex: p.lex.fileIdx,
    line: p.lineNumberPrevious,
    col: p.lex.previousTokenEnd.col,
  )

proc drainSkipped(p: var Parser, indent: int): seq[Token] =
  for c in p.skipped:
    if c.indent == -1 or c.indent > indent:
      result.add(c)
      p.lineNumberPrevious = int c.lineB
    else:
      break

  if result.high >= 0:
    p.skipped.delete(0 .. result.high)

proc splitLookahead(p: var Parser, n: PNode, indent: int, commentLoc: CommentLoc) =
  for c in p.drainSkipped(indent):
    n.addComment(c, commentLoc)

proc splitLookahead(p: var Parser, n: PNode, commentLoc: CommentLoc) =
  # Split comments such that anything at greater indent than the next non-comment token
  # is added to `n` at `commentLoc`
  splitLookahead(p, n, p.tok.indent, commentLoc)

proc addSkipped(p: var Parser, n: PNode) =
  # Add skipped comments at current or greater indent level to n as comment
  # nodes - it is best used after moving greater-indent level nodes to a postfix
  # to avoid them moving around

  var tmp = move(p.skipped)
  var i = 0
  while i < tmp.len and (tmp[i].indent < 0 or tmp[i].indent >= p.currInd):
    let c = newNodeI(nkCommentStmt, getLineInfo(p.lex, tmp[i]))
    p.lineNumberPrevious = int tmp[i].lineB
    setEndInfo(c)
    c.strVal = tmp[i].literal
    n.add c
    i += 1

  p.skipped = tmp[i ..^ 1]

proc indAndComment(p: var Parser, n: PNode, maybeMissEquals = false) =
  # indComment(p, n, commentLoc)
  if p.tok.indent > p.currInd:
    if maybeMissEquals:
      let col = p.bufposPrevious - p.lineStartPrevious
      var info = newLineInfo(p.lex.fileIdx, p.lineNumberPrevious, col)
      parMessage(
        p, "invalid indentation, maybe you forgot a '=' at $1 ?" % [p.lex.config $ info]
      )
    else:
      #raiseAssert $p.tok, " ", p.parLineInfo()
      parMessage(p, errInvalidIndentation % "indAndComment")

proc newNodeP(kind: TNodeKind, p: var Parser, withPrefix = true): PNode =
  result = newNodeI(kind, parLineInfo(p))
  if withPrefix:
    result.prefix = move(p.skipped)

proc newIntNodeP(kind: TNodeKind, intVal: BiggestInt, p: var Parser): PNode =
  result = newNodeP(kind, p)
  result.intVal = intVal

proc newFloatNodeP(kind: TNodeKind, floatVal: BiggestFloat, p: var Parser): PNode =
  result = newNodeP(kind, p)
  result.floatVal = floatVal

proc newStrNodeP(kind: TNodeKind, strVal: string, p: var Parser): PNode =
  result = newNodeP(kind, p)
  result.strVal = strVal

proc newIdentNodeP(ident: PIdent, p: var Parser): PNode =
  result = newNodeP(nkIdent, p)
  result.ident = ident

proc parseExpr(p: var Parser): PNode
proc parseStmt(p: var Parser, allowEmpty = false): PNode
proc parseTypeDesc(p: var Parser, fullExpr = false): PNode
proc parseTypeDefValue(p: var Parser): PNode

proc isSigilLike(tok: Token): bool {.inline.} =
  result = tok.tokType == tkOpr and tok.ident.s[0] == '@'

proc isRightAssociative(tok: Token): bool {.inline.} =
  ## Determines whether the token is right assocative.
  result = tok.tokType == tkOpr and tok.ident.s[0] == '^'
  # or (tok.ident.s.len > 1 and tok.ident.s[^1] == '>')

proc wrap(a, b: PNode): PNode =
  a.info = b.info
  a.endInfo = b.endInfo
  a.prefix = move(b.prefix)
  a.add(b)
  a

proc isUnary(tok: Token): bool =
  ## Check if the given token is a unary operator
  tok.tokType in {tkOpr, tkDotDot} and tok.spacing == {tsLeading}

proc checkBinary(p: Parser) {.inline.} =
  ## Check if the current parser token is a binary operator.
  # we don't check '..' here as that's too annoying
  if p.tok.tokType == tkOpr:
    if p.tok.spacing == {tsTrailing}:
      parMessage(p, warnInconsistentSpacing, prettyTok(p.tok))

#| module = complexOrSimpleStmt ^* (';' / IND{=})
#|
#| comma = ',' COMMENT?
#| semicolon = ';' COMMENT?
#| colon = ':' COMMENT?
#| colcom = ':' COMMENT?
#|
#| operator =  OP0 | OP1 | OP2 | OP3 | OP4 | OP5 | OP6 | OP7 | OP8 | OP9
#|          | 'or' | 'xor' | 'and'
#|          | 'is' | 'isnot' | 'in' | 'notin' | 'of' | 'as' | 'from'
#|          | 'div' | 'mod' | 'shl' | 'shr' | 'not' | '..'
#|
#| prefixOperator = operator
#|
#| optInd = COMMENT? IND?
#| optPar = (IND{>} | IND{=})?
#|
#| simpleExpr = arrowExpr (OP0 optInd arrowExpr)* pragma?
#| arrowExpr = assignExpr (OP1 optInd assignExpr)*
#| assignExpr = orExpr (OP2 optInd orExpr)*
#| orExpr = andExpr (OP3 optInd andExpr)*
#| andExpr = cmpExpr (OP4 optInd cmpExpr)*
#| cmpExpr = sliceExpr (OP5 optInd sliceExpr)*
#| sliceExpr = ampExpr (OP6 optInd ampExpr)*
#| ampExpr = plusExpr (OP7 optInd plusExpr)*
#| plusExpr = mulExpr (OP8 optInd mulExpr)*
#| mulExpr = dollarExpr (OP9 optInd dollarExpr)*
#| dollarExpr = primary (OP10 optInd primary)*
proc isOperator(tok: Token): bool =
  #| operatorB = OP0 | OP1 | OP2 | OP3 | OP4 | OP5 | OP6 | OP7 | OP8 | OP9 |
  #|             'div' | 'mod' | 'shl' | 'shr' | 'in' | 'notin' |
  #|             'is' | 'isnot' | 'not' | 'of' | 'as' | 'from' | '..' | 'and' | 'or' | 'xor'
  tok.tokType in {
    tkOpr, tkDiv, tkMod, tkShl, tkShr, tkIn, tkNotin, tkIs, tkIsnot, tkNot, tkOf, tkAs,
    tkFrom, tkDotDot, tkAnd, tkOr, tkXor,
  }

proc parseComStmt(p: var Parser, n: PNode, commentLoc = clPostfix): PNode =
  splitLookahead(p, n, int.high, commentLoc)

  # Let parseStmt deal with statement lists that contain only doc comments
  if not (
    p.tok.indent >= 0 and p.tok.indent < p.currInd and p.skipped.len > 0 and
    p.skipped[0].indent > p.currInd
  ):
    splitLookahead(p, n, commentLoc)

  parseStmt(p, allowEmpty = true)

proc parseColComStmt(p: var Parser, n: PNode, commentLoc = clPostfix): PNode =
  # `:` followed by a list of statements, with comments interspresed
  # Comments with greater indent than the statements are added to `n` at
  # `commentLoc` - these typically are part of what was just parsed.
  #
  # Comments at the statement level are added as prefixes to the statement
  # since they are more likely to be part of whatever the statements are doing.
  # This strategy requires lookahead for the comment tokents.
  eat(p, tkColon)
  parseComStmt(p, n, commentLoc)

const tkBuiltInMagics = {tkType, tkStatic, tkAddr}

template setEndInfo() =
  setEndInfo(result)

proc parseSymbol(p: var Parser, mode = smNormal): PNode =
  #| symbol = '`' (KEYW|IDENT|literal|(operator|'('|')'|'['|']'|'{'|'}'|'=')+)+ '`'
  #|        | IDENT | 'addr' | 'type' | 'static'
  #| symbolOrKeyword = symbol | KEYW
  case p.tok.tokType
  of tkSymbol:
    result = newIdentNodeP(p.tok.ident, p)
    getTok(p)
  of tokKeywordLow .. tokKeywordHigh:
    if p.tok.tokType in tkBuiltInMagics or mode == smAfterDot:
      # for backwards compatibility these 2 are always valid:
      result = newIdentNodeP(p.tok.ident, p)
      getTok(p)
    elif p.tok.tokType == tkNil and mode == smAllowNil:
      result = newNodeP(nkNilLit, p)
      getTok(p)
    else:
      parMessage(p, errIdentifierExpected, p.tok)
      result = p.emptyNode
  of tkAccent:
    result = newNodeP(nkAccQuoted, p)
    getTok(p)
    # progress guaranteed
    while true:
      case p.tok.tokType
      of tkAccent:
        if result.len == 0:
          parMessage(p, errIdentifierExpected, p.tok)
        break
      of tkOpr, tkDot, tkDotDot, tkEquals, tkParLe .. tkParDotRi:
        var lineinfo = parLineInfo(p)
        var accm = ""
        while p.tok.tokType in {tkOpr, tkDot, tkDotDot, tkEquals, tkParLe .. tkParDotRi}:
          accm.add($p.tok)
          lineinfo.offsetB = p.tok.offsetB
          getTok(p)
        let node = newNodeI(nkIdent, lineinfo)
        node.ident = p.lex.cache.getIdent(accm)
        result.add(node)
      of tokKeywordLow .. tokKeywordHigh, tkSymbol, tkIntLit .. tkCustomLit:
        result.add(newIdentNodeP(p.lex.cache.getIdent($p.tok), p))
        getTok(p)
      else:
        parMessage(p, errIdentifierExpected, p.tok)
        break
    eat(p, tkAccent)
  else:
    parMessage(p, errIdentifierExpected, p.tok)
    # BUGFIX: We must consume a token here to prevent endless loops!
    # But: this really sucks for idetools and keywords, so we don't do it
    # if it is a keyword:
    #if not isKeyword(p.tok.tokType): getTok(p)
    result = p.emptyNode
  setEndInfo()

proc equals(p: var Parser, a: PNode): PNode =
  if p.tok.tokType == tkEquals:
    result = wrap(newNodeP(nkExprEqExpr, p), a)
    getTok(p)
    result.add(parseExpr(p))
  else:
    result = a

proc colonOrEquals(p: var Parser, a: PNode): PNode =
  if p.tok.tokType == tkColon:
    result = wrap(newNodeP(nkExprColonExpr, p), a)
    getTok(p)
    result.add(parseExpr(p))
  else:
    result = equals(p, a)

proc exprColonEqExpr(p: var Parser): PNode =
  #| exprColonEqExpr = expr ((':'|'=') expr
  #|                        / doBlock extraPostExprBlock*)?
  var a = parseExpr(p)
  if p.tok.tokType == tkDo:
    result = postExprBlocks(p, a)
  else:
    result = colonOrEquals(p, a)

proc exprEqExpr(p: var Parser): PNode =
  #| exprEqExpr = expr ('=' expr
  #|                   / doBlock extraPostExprBlock*)?
  var a = parseExpr(p)
  if p.tok.tokType == tkDo:
    result = postExprBlocks(p, a)
  else:
    result = equals(p, a)

proc exprList(p: var Parser, endTok: TokType, result: PNode) =
  #| exprList = expr ^+ comma
  getTok(p)
  splitLookahead(p, result, clMid)
  optInd(p, result)
  # progress guaranteed
  var a = parseExpr(p)
  result.add(a)
  while (p.tok.tokType != endTok) and (p.tok.tokType != tkEof):
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, result[^1], clPostfix)
    optInd(p, a)
    var a = parseExpr(p)
    result.add(a)

proc optionalExprList(p: var Parser, endTok: TokType, result: PNode) =
  #| optionalExprList = expr ^* comma
  getTok(p)
  splitLookahead(p, result, clMid)
  optInd(p, result)
  # progress guaranteed
  while (p.tok.tokType != endTok) and (p.tok.tokType != tkEof):
    var a = parseExpr(p)
    result.add(a)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)

proc exprColonEqExprListAux(p: var Parser, endTok: TokType, result: PNode) =
  assert(endTok in {tkCurlyRi, tkCurlyDotRi, tkBracketRi, tkParRi})
  getTok(p)
  splitLookahead(p, result, clMid)
  optPar(p)
  # progress guaranteed
  while p.tok.tokType != endTok and p.tok.tokType != tkEof:
    var a = exprColonEqExpr(p)

    result.add(a)
    splitLookahead(p, a, clPostfix)
    if p.tok.tokType != tkComma:
      # Attach the last comments before the closing token as a postfix to the last
      # item (which is not ideal due to the extra indent, but at least it comes
      # before the closing token this way)
      a.postfix.add move(p.skipped)
      break
    elif result.kind == nkPar:
      result.transitionSonsKind(nkTupleConstr)

    getTok(p)
    splitLookahead(p, a, clPostfix)

  optPar(p)
  eat(p, endTok)

proc exprColonEqExprList(p: var Parser, kind: TNodeKind, endTok: TokType): PNode =
  #| exprColonEqExprList = exprColonEqExpr (comma exprColonEqExpr)* (comma)?
  result = newNodeP(kind, p)

  exprColonEqExprListAux(p, endTok, result)

proc dotExpr(p: var Parser, a: PNode): PNode =
  var info = p.parLineInfo

  getTok(p)

  result = wrap(newNodeP(nkDotExpr, p, withPrefix = false), a)
  result.info = info
  splitLookahead(p, result, clMid)
  optInd(p, result)
  result.add(parseSymbol(p, smAfterDot))
  # TODO non-line-ending comments?
  # splitLookahead(p, result[^1], clPostfix)
  if p.tok.tokType == tkBracketLeColon and tsLeading notin p.tok.spacing:
    var x = newNodeP(nkBracketExpr, p)
    # rewrite 'x.y[:z]()' to 'y[z](x)'
    x.add result[1]

    exprList(p, tkBracketRi, x)
    eat(p, tkBracketRi)
    var y = wrap(newNodeP(nkCall, p), x)
    y.add result[0]
    if p.tok.tokType == tkParLe and tsLeading notin p.tok.spacing:
      exprColonEqExprListAux(p, tkParRi, y)

    result = y

proc dotLikeExpr(p: var Parser, a: PNode): PNode =
  result = newNodeI(nkInfix, a.info)
  optInd(p, result)
  var opNode = newIdentNodeP(p.tok.ident, p)
  getTok(p)
  result.add(opNode)
  result.add(a)
  result.add(parseSymbol(p, smAfterDot))
  setEndInfo(result)

proc qualifiedIdent(p: var Parser): PNode =
  #| qualifiedIdent = symbol ('.' optInd symbolOrKeyword)?
  result = parseSymbol(p)
  if p.tok.tokType == tkDot:
    splitLookahead(p, result, clPostfix)
    result = dotExpr(p, result)

proc setOrTableConstr(p: var Parser): PNode =
  #| setOrTableConstr = '{' ((exprColonEqExpr comma)* | ':' ) '}'
  result = newNodeP(nkCurly, p)
  getTok(p) # skip '{'

  splitLookahead(p, result, clMid)
  optInd(p, result)
  if p.tok.tokType == tkColon:
    getTok(p) # skip ':'
    result.transitionSonsKind(nkTableConstr)
  else:
    # progress guaranteed
    while p.tok.tokType notin {tkCurlyRi, tkEof}:
      var a = exprColonEqExpr(p)
      if a.kind == nkExprColonExpr:
        result.transitionSonsKind(nkTableConstr)
      result.add(a)
      if p.tok.tokType != tkComma:
        # All comments before closing token
        a.postfix.add move(p.skipped)
        break
      getTok(p)
      splitLookahead(p, a, clPostfix)
  optPar(p)
  eat(p, tkCurlyRi) # skip '}'

proc parseCast(p: var Parser): PNode =
  #| castExpr = 'cast' ('[' optInd typeDesc optPar ']' '(' optInd expr optPar ')') /
  #                    ('(' optInd exprColonEqExpr optPar ')')
  result = newNodeP(nkCast, p)

  getTok(p)
  if p.tok.tokType == tkBracketLe:
    getTok(p)
    optInd(p, result)
    result.add(parseTypeDesc(p))
    optPar(p)
    eat(p, tkBracketRi)
    eat(p, tkParLe)
    optInd(p, result)
    result.add(parseExpr(p))
  else:
    result.add p.emptyNode
    eat(p, tkParLe)
    optInd(p, result)
    result.add(exprColonEqExpr(p))
  optPar(p)
  eat(p, tkParRi)
  setEndInfo()

proc setBaseFlags(n: PNode, base: NumericalBase) =
  case base
  of base10:
    discard
  of base2:
    incl(n.flags, nfBase2)
  of base8:
    incl(n.flags, nfBase8)
  of base16:
    incl(n.flags, nfBase16)

proc parseGStrLit(p: var Parser, a: PNode): PNode =
  case p.tok.tokType
  of tkGStrLit:
    result = newNodeP(nkCallStrLit, p)
    result.add(a)
    result.add(newStrNodeP(nkRStrLit, p.tok.literal, p))
    getTok(p)
  of tkGTripleStrLit:
    result = newNodeP(nkCallStrLit, p)
    result.add(a)
    result.add(newStrNodeP(nkTripleStrLit, p.tok.literal, p))
    getTok(p)
  else:
    result = a
  setEndInfo()

proc complexOrSimpleStmt(p: var Parser): PNode
proc simpleExpr(p: var Parser, mode = pmNormal): PNode
proc parseIfOrWhenExpr(p: var Parser, kind: TNodeKind): PNode

proc semiStmtList(p: var Parser, result: PNode) =
  inc p.inSemiStmtList
  withInd(p):
    # Be lenient with the first stmt/expr
    let a =
      case p.tok.tokType
      of tkIf:
        parseIfOrWhenExpr(p, nkIfStmt)
      of tkWhen:
        parseIfOrWhenExpr(p, nkWhenStmt)
      else:
        complexOrSimpleStmt(p)
    result.add a

    while p.tok.tokType != tkEof:
      if p.tok.tokType == tkSemiColon:
        getTok(p)
      if p.tok.tokType == tkParRi:
        break
      elif not (sameInd(p) or realInd(p)):
        parMessage(p, errInvalidIndentation % "semiStmtList")
      let a = complexOrSimpleStmt(p)
      if a.kind == nkEmpty:
        parMessage(p, errExprExpected & " (semiStmtList)", p.tok)
        getTok(p)
      else:
        result.add a
  dec p.inSemiStmtList
  result.transitionSonsKind(nkStmtListExpr)

proc parsePar(p: var Parser): PNode =
  #| parKeyw = 'discard' | 'include' | 'if' | 'while' | 'case' | 'try'
  #|         | 'finally' | 'except' | 'for' | 'block' | 'const' | 'let'
  #|         | 'when' | 'var' | 'mixin'
  #| par = '(' optInd
  #|           ( &parKeyw (ifExpr / complexOrSimpleStmt) ^+ ';'
  #|           | ';' (ifExpr / complexOrSimpleStmt) ^+ ';'
  #|           | pragmaStmt
  #|           | simpleExpr ( (doBlock extraPostExprBlock*)
  #|                        | ('=' expr (';' (ifExpr / complexOrSimpleStmt) ^+ ';' )? )
  #|                        | (':' expr (',' exprColonEqExpr     ^+ ',' )? ) ) )
  #|           optPar ')'
  #
  # unfortunately it's ambiguous: (expr: expr) vs (exprStmt); however a
  # leading ';' could be used to enforce a 'stmt' context ...
  result = newNodeP(nkPar, p)
  getTok(p)
  optInd(p, result)

  if p.tok.tokType in {
    tkDiscard, tkInclude, tkIf, tkWhile, tkCase, tkTry, tkDefer, tkFinally, tkExcept,
    tkBlock, tkConst, tkLet, tkWhen, tkVar, tkFor, tkMixin,
  }:
    # XXX 'bind' used to be an expression, so we exclude it here;
    # tests/reject/tbind2 fails otherwise.
    semiStmtList(p, result)
  elif p.tok.tokType == tkSemiColon:
    # '(;' enforces 'stmt' context:
    getTok(p)
    optInd(p, result)
    semiStmtList(p, result)
  elif p.tok.tokType == tkCurlyDotLe:
    result.add(parseStmtPragma(p))
  elif p.tok.tokType == tkParRi:
    # Empty tuple '()'
    result.transitionSonsKind(nkTupleConstr)
  else:
    var a = simpleExpr(p)
    if p.tok.tokType == tkDo:
      result = postExprBlocks(p, a)
    elif p.tok.tokType == tkEquals:
      # special case: allow assignments
      let asgn = newNodeP(nkAsgn, p)
      getTok(p)
      optInd(p, result)
      let b = parseExpr(p)
      asgn.add a
      asgn.add b
      result.add(asgn)
      if p.tok.tokType == tkSemiColon:
        semiStmtList(p, result)
    elif p.tok.tokType == tkSemiColon:
      # stmt context:
      result.add(a)
      semiStmtList(p, result)
    else:
      a = colonOrEquals(p, a)
      if a.kind == nkExprColonExpr:
        result.transitionSonsKind(nkTupleConstr)
      result.add(a)
      if p.tok.tokType == tkComma:
        getTok(p)
        splitLookahead(p, a, clPostfix)

        # (1,) produces a tuple expression:
        result.transitionSonsKind(nkTupleConstr)
        # progress guaranteed
        while p.tok.tokType != tkParRi and p.tok.tokType != tkEof:
          var a = exprColonEqExpr(p)
          result.add(a)
          if p.tok.tokType != tkComma:
            break
          getTok(p)
          splitLookahead(p, a, clPostfix)
      # All comments before closing token
      result[^1].postfix.add move(p.skipped)
  optPar(p)
  eat(p, tkParRi)
  setEndInfo()

proc identOrLiteral(p: var Parser, mode: PrimaryMode): PNode =
  #| literal = | INT_LIT | INT8_LIT | INT16_LIT | INT32_LIT | INT64_LIT
  #|           | UINT_LIT | UINT8_LIT | UINT16_LIT | UINT32_LIT | UINT64_LIT
  #|           | FLOAT_LIT | FLOAT32_LIT | FLOAT64_LIT
  #|           | STR_LIT | RSTR_LIT | TRIPLESTR_LIT
  #|           | CHAR_LIT | CUSTOM_NUMERIC_LIT
  #|           | NIL
  #| generalizedLit = GENERALIZED_STR_LIT | GENERALIZED_TRIPLESTR_LIT
  #| identOrLiteral = generalizedLit | symbol | literal
  #|                | par | arrayConstr | setOrTableConstr | tupleConstr
  #|                | castExpr
  #| tupleConstr = '(' optInd (exprColonEqExpr comma?)* optPar ')'
  #| arrayConstr = '[' optInd (exprColonEqExpr comma?)* optPar ']'
  case p.tok.tokType
  of tkSymbol, tkBuiltInMagics, tkOut:
    result = newIdentNodeP(p.tok.ident, p)
    getTok(p)
    result = parseGStrLit(p, result)
  of tkAccent:
    result = parseSymbol(p) # literals
  of tkIntLit:
    result = newIntNodeP(nkIntLit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkInt8Lit:
    result = newIntNodeP(nkInt8Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkInt16Lit:
    result = newIntNodeP(nkInt16Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkInt32Lit:
    result = newIntNodeP(nkInt32Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkInt64Lit:
    result = newIntNodeP(nkInt64Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkUIntLit:
    result = newIntNodeP(nkUIntLit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkUInt8Lit:
    result = newIntNodeP(nkUInt8Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkUInt16Lit:
    result = newIntNodeP(nkUInt16Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkUInt32Lit:
    result = newIntNodeP(nkUInt32Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkUInt64Lit:
    result = newIntNodeP(nkUInt64Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkFloatLit:
    result = newFloatNodeP(nkFloatLit, p.tok.fNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkFloat32Lit:
    result = newFloatNodeP(nkFloat32Lit, p.tok.fNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkFloat64Lit:
    result = newFloatNodeP(nkFloat64Lit, p.tok.fNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkFloat128Lit:
    result = newFloatNodeP(nkFloat128Lit, p.tok.fNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of tkStrLit:
    result = newStrNodeP(nkStrLit, p.tok.literal, p)
    getTok(p)
  of tkRStrLit:
    result = newStrNodeP(nkRStrLit, p.tok.literal, p)
    getTok(p)
  of tkTripleStrLit:
    result = newStrNodeP(nkTripleStrLit, p.tok.literal, p)
    getTok(p)
  of tkCharLit:
    result = newIntNodeP(nkCharLit, ord(p.tok.literal[0]), p)
    getTok(p)
  of tkCustomLit:
    let splitPos = p.tok.iNumber.int
    let str = newStrNodeP(nkRStrLit, p.tok.literal.substr(0, splitPos - 1), p)
    let callee = newIdentNodeP(getIdent(p.lex.cache, p.tok.literal.substr(splitPos)), p)

    callee.info.offsetA += splitPos

    result = newNodeP(nkDotExpr, p)
    result.add str
    result.add callee
    getTok(p)
  of tkNil:
    result = newNodeP(nkNilLit, p)
    getTok(p)
  of tkParLe:
    # () constructor
    if mode in {pmTypeDesc, pmTypeDef}:
      result = exprColonEqExprList(p, nkPar, tkParRi)
    else:
      result = parsePar(p)
  of tkCurlyLe:
    # {} constructor
    result = setOrTableConstr(p)
  of tkBracketLe:
    # [] constructor
    result = exprColonEqExprList(p, nkBracket, tkBracketRi)
  of tkCast:
    result = parseCast(p)
  else:
    parMessage(p, errExprExpected & " (identOrLiteral)", p.tok)
    getTok(p) # we must consume a token here to prevent endless loops!
    result = p.emptyNode

proc namedParams(
    p: var Parser, callee: PNode, kind: TNodeKind, endTok: TokType
): PNode =
  let a = callee
  result = wrap(newNodeP(kind, p), a)
  # progress guaranteed
  exprColonEqExprListAux(p, endTok, result)

proc commandParam(p: var Parser, isFirstParam: var bool, mode: PrimaryMode): PNode =
  if mode == pmTypeDesc:
    result = simpleExpr(p, mode)
  elif not isFirstParam:
    result = exprEqExpr(p)
  else:
    result = parseExpr(p)
    if p.tok.tokType == tkDo:
      result = postExprBlocks(p, result)
  isFirstParam = false

proc commandExpr(p: var Parser, r: PNode, mode: PrimaryMode): PNode =
  if mode == pmTrySimple:
    result = r
  else:
    result = wrap(newNodeP(nkCommand, p), r)
    let baseIndent = p.currInd
    var isFirstParam = true
    # progress NOT guaranteed
    p.hasProgress = false
    result.add commandParam(p, isFirstParam, mode)
    splitLookahead(p, result[^1], baseIndent, clPostfix)

proc isDotLike(tok: Token): bool =
  result =
    tok.tokType == tkOpr and tok.ident.s.len > 1 and tok.ident.s[0] == '.' and
    tok.ident.s[1] != '.'

proc primarySuffix(p: var Parser, r: PNode, baseIndent: int, mode: PrimaryMode): PNode =
  #| primarySuffix = '(' (exprColonEqExpr comma?)* ')'
  #|       | '.' optInd symbolOrKeyword ('[:' exprList ']' ( '(' exprColonEqExpr ')' )?)? generalizedLit?
  #|       | DOTLIKEOP optInd symbolOrKeyword generalizedLit?
  #|       | '[' optInd exprColonEqExprList optPar ']'
  #|       | '{' optInd exprColonEqExprList optPar '}'
  # XXX strong spaces need to be reflected above
  result = r

  # progress guaranteed
  while p.tok.indent < 0 or (p.tok.tokType == tkDot and p.tok.indent >= baseIndent):
    case p.tok.tokType
    of tkParLe:
      # progress guaranteed
      if tsLeading in p.tok.spacing:
        result = commandExpr(p, result, mode)
        break
      result = namedParams(p, result, nkCall, tkParRi)
      # After a parLe, it's tricky to inject doc comments so we'll move them to
      # the next token
      # TODO investigate non-doc-comments and empty par
      if result.len > 1 and result[1].kind == nkExprColonExpr:
        result.transitionSonsKind(nkObjConstr)
    of tkDot:
      # progress guaranteed
      result = dotExpr(p, result)
      result = parseGStrLit(p, result)
    of tkBracketLe:
      # progress guaranteed
      if tsLeading in p.tok.spacing:
        result = commandExpr(p, result, mode)
        break
      result = namedParams(p, result, nkBracketExpr, tkBracketRi)
    of tkCurlyLe:
      # progress guaranteed
      if tsLeading in p.tok.spacing:
        result = commandExpr(p, result, mode)
        break
      result = namedParams(p, result, nkCurlyExpr, tkCurlyRi)
    of tkSymbol,
        tkAccent,
        tkIntLit .. tkCustomLit,
        tkNil,
        tkCast,
        tkOpr,
        tkDotDot,
        tkVar,
        tkOut,
        tkStatic,
        tkType,
        tkEnum,
        tkTuple,
        tkObject,
        tkProc:
      # XXX: In type sections we allow the free application of the
      # command syntax, with the exception of expressions such as
      # `foo ref` or `foo ptr`. Unfortunately, these two are also
      # used as infix operators for the memory regions feature and
      # the current parsing rules don't play well here.
      let isDotLike2 = p.tok.isDotLike
      if isDotLike2 and p.lex.config.isDefined("nimPreviewDotLikeOps"):
        # synchronize with `tkDot` branch
        result = dotLikeExpr(p, result)
        result = parseGStrLit(p, result)
      else:
        if isDotLike2:
          parMessage(
            p, warnDotLikeOps,
            "dot-like operators will be parsed differently with `-d:nimPreviewDotLikeOps`",
          )
        if p.inPragma == 0 and (isUnary(p.tok) or p.tok.tokType notin {tkOpr, tkDotDot}):
          # actually parsing {.push hints:off.} as {.push(hints:off).} is a sweet
          # solution, but pragmas.nim can't handle that
          result = commandExpr(p, result, mode)
        break
    else:
      break

proc parseOperators(
    p: var Parser, headNode: PNode, limit: int, mode: PrimaryMode
): PNode =
  result = headNode
  # expand while operators have priorities higher than 'limit'
  var opPrec = getPrecedence(p.tok)
  let modeB = if mode == pmTypeDef: pmTypeDesc else: mode
  # the operator itself must not start on a new line:
  # progress guaranteed
  while opPrec >= limit and p.tok.indent < 0 and not isUnary(p.tok):
    checkBinary(p)
    let leftAssoc = ord(not isRightAssociative(p.tok))
    var a = newNodeP(nkInfix, p)
    var opNode = newIdentNodeP(p.tok.ident, p) # skip operator:
    getTok(p)
    splitLookahead(p, opNode, clPostfix)
    optPar(p)
    # read sub-expression with higher priority:
    var b = simpleExprAux(p, opPrec + leftAssoc, modeB)
    a.add(opNode)
    a.add(result)
    a.add(b)
    # Reset the "beginning" of the infix to capture empty lines correctly
    a.info = result.info

    result = a
    opPrec = getPrecedence(p.tok)
  setEndInfo()

proc simpleExprAux(p: var Parser, limit: int, mode: PrimaryMode): PNode =
  var mode = mode
  result = primary(p, mode)
  if mode == pmTrySimple:
    mode = pmNormal
  if p.tok.tokType == tkCurlyDotLe and (p.tok.indent < 0 or realInd(p)) and
      mode == pmNormal:
    var pragmaExp = newNodeP(nkPragmaExpr, p, withPrefix = false)
    pragmaExp.add result
    pragmaExp.add p.parsePragma
    result = pragmaExp
  result = parseOperators(p, result, limit, mode)

proc simpleExpr(p: var Parser, mode = pmNormal): PNode =
  result = simpleExprAux(p, -1, mode)

proc parsePragma(p: var Parser): PNode =
  #| pragma = '{.' optInd (exprColonEqExpr comma?)* optPar ('.}' | '}')
  result = newNodeP(nkPragma, p)
  inc p.inPragma
  getTok(p)
  splitLookahead(p, result, clMid)
  optInd(p, result)
  while p.tok.tokType notin {tkCurlyDotRi, tkCurlyRi, tkEof}:
    p.hasProgress = false
    var a = exprColonEqExpr(p)
    splitLookahead(p, a, clPostfix)
    if not p.hasProgress:
      break
    result.add(a)
    if p.tok.tokType == tkComma:
      getTok(p)
      splitLookahead(p, a, clPostfix)
  optPar(p)
  if p.tok.tokType in {tkCurlyDotRi, tkCurlyRi}:
    getTok(p)
  else:
    parMessage(p, "expected '.}'")
  dec p.inPragma
  setEndInfo()

proc identVis(p: var Parser, allowDot = false): PNode =
  #| identVis = symbol OPR?  # postfix position
  #| identVisDot = symbol '.' optInd symbolOrKeyword OPR?
  var a = parseSymbol(p)
  splitLookahead(p, a, clPostfix)
  if p.tok.tokType == tkOpr:
    result = newNodeP(nkPostfix, p)
    result.add(newIdentNodeP(p.tok.ident, p))
    result.add(a)
    getTok(p)
  elif p.tok.tokType == tkDot and allowDot:
    result = dotExpr(p, a)
  else:
    result = a

proc identWithPragma(p: var Parser, allowDot = false): PNode =
  #| identWithPragma = identVis pragma?
  #| identWithPragmaDot = identVisDot pragma?
  var a = identVis(p, allowDot)
  if p.tok.tokType == tkCurlyDotLe:
    result = newNodeP(nkPragmaExpr, p, withPrefix = false)
    result.add(a)
    result.add(parsePragma(p))
  else:
    result = a

type
  DeclaredIdentFlag = enum
    withPragma # identifier may have pragma
    withBothOptional # both ':' and '=' parts are optional
    withDot # allow 'var ident.ident = value'

  DeclaredIdentFlags = set[DeclaredIdentFlag]

proc parseIdentColonEquals(p: var Parser, flags: DeclaredIdentFlags): PNode =
  #| declColonEquals = identWithPragma (comma identWithPragma)* comma?
  #|                   (':' optInd typeDescExpr)? ('=' optInd expr)?
  #| identColonEquals = IDENT (comma IDENT)* comma?
  #|      (':' optInd typeDescExpr)? ('=' optInd expr)?)
  var a: PNode
  result = newNodeP(nkIdentDefs, p)
  # progress guaranteed
  while true:
    case p.tok.tokType
    of tkSymbol, tkAccent:
      if withPragma in flags:
        a = identWithPragma(p, allowDot = withDot in flags)
      else:
        a = parseSymbol(p)
      if a.kind == nkEmpty:
        return
    else:
      break
    result.add(a)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)

  # We let comments sit as prefixes to whatever comes next - this works well
  # with tkColon / tkEquals which don't have indent requirements..
  if p.tok.tokType == tkColon:
    getTok(p)
    optInd(p, result)
    result.add(parseTypeDesc(p, fullExpr = true))
  else:
    result.add p.emptyNode
    if p.tok.tokType != tkEquals and withBothOptional notin flags:
      parMessage(p, "':' or '=' expected, but got '$1'", p.tok)
  if p.tok.tokType == tkEquals:
    getTok(p)
    optInd(p, result)
    result.add(parseExpr(p))
  else:
    result.add p.emptyNode
  setEndInfo()

proc parseTuple(p: var Parser, indentAllowed = false): PNode =
  #| tupleTypeBracket = '[' optInd (identColonEquals (comma/semicolon)?)* optPar ']'
  #| tupleType = 'tuple' tupleTypeBracket
  #| tupleDecl = 'tuple' (tupleTypeBracket /
  #|     COMMENT? (IND{>} identColonEquals (IND{=} identColonEquals)*)?)
  result = newNodeP(nkTupleTy, p)
  getTok(p)
  if p.tok.tokType == tkBracketLe:
    getTok(p)
    optInd(p, result)
    # progress guaranteed
    while p.tok.tokType in {tkSymbol, tkAccent}:
      var a = parseIdentColonEquals(p, {})
      result.add(a)
      if p.tok.tokType notin {tkComma, tkSemiColon}:
        # All comments before closing token
        a.postfix.add move(p.skipped)
        break
      getTok(p)
      splitLookahead(p, a, clPostfix)
    optPar(p)
    eat(p, tkBracketRi)
  elif indentAllowed:
    splitLookahead(p, result, clMid)
    if realInd(p):
      withInd(p):
        splitLookahead(p, result, clMid)
        # progress guaranteed
        while true:
          case p.tok.tokType
          of tkSymbol, tkAccent:
            var a = parseIdentColonEquals(p, {})
            splitLookahead(p, a, clPostfix)
            result.add(a)
          of tkEof:
            break
          else:
            parMessage(p, errIdentifierExpected, p.tok)
            break
          if not sameInd(p):
            break
  elif p.tok.tokType == tkParLe:
    parMessage(
      p, errGenerated, "the syntax for tuple types is 'tuple[...]', not 'tuple(...)'"
    )
  else:
    result = newNodeP(nkTupleClassTy, p)
    splitLookahead(p, result, clPostfix)
  setEndInfo()

proc parseParamList(p: var Parser, retColon = true): PNode =
  #| paramList = '(' declColonEquals ^* (comma/semicolon) ')'
  #| paramListArrow = paramList? ('->' optInd typeDesc)?
  #| paramListColon = paramList? (':' optInd typeDesc)?
  var a: PNode
  result = newNodeP(nkFormalParams, p, withPrefix = false)
  result.add(p.emptyNode) # return type
  let hasParLe = p.tok.tokType == tkParLe and p.tok.indent < 0
  if hasParLe:
    getTok(p)
    splitLookahead(p, result, clMid)
    optInd(p, result)
    # progress guaranteed
    while true:
      case p.tok.tokType
      of tkSymbol, tkAccent:
        a = parseIdentColonEquals(p, {withBothOptional, withPragma})
      of tkParRi:
        break
      of tkVar:
        parMessage(
          p, errGenerated, "the syntax is 'parameter: var T', not 'var parameter: T'"
        )
        break
      else:
        parMessage(p, "expected closing ')'")
        break
      result.add(a)
      if p.tok.tokType notin {tkComma, tkSemiColon}:
        # All comments before closing token
        a.postfix.add move(p.skipped)
        break
      getTok(p)
      splitLookahead(p, a, clPostfix)
    # Not ideal, but we'll attach the last comment in the par to the last
    # parameter
    if result.len > 0:
      splitLookahead(p, result[^1], clPostfix)
    optPar(p)
    eat(p, tkParRi)
  let hasRet =
    if retColon:
      p.tok.tokType == tkColon
    else:
      p.tok.tokType == tkOpr and p.tok.ident.s == "->"
  if hasRet and p.tok.indent < 0:
    getTok(p)
    optInd(p, result)
    result[0] = parseTypeDesc(p)
  elif not retColon and not hasParLe:
    # Mark as "not there" in order to mark for deprecation in the semantic pass:
    result = p.emptyNode
  setEndInfo()

proc optPragmas(p: var Parser): PNode =
  if p.tok.tokType == tkCurlyDotLe and (p.tok.indent < 0 or realInd(p)):
    result = parsePragma(p)
  else:
    result = p.emptyNode

proc parseDoBlock(p: var Parser, info: TLineInfo): PNode =
  #| doBlock = 'do' paramListArrow pragma? colcom stmt
  var params = parseParamList(p, retColon = false)
  let pragmas = optPragmas(p)
  result = newProcNode(
    nkDo,
    info,
    body = nil,
    params = params,
    name = p.emptyNode,
    pattern = p.emptyNode,
    genericParams = p.emptyNode,
    pragmas = pragmas,
    exceptions = p.emptyNode,
  )
  let body = parseColComStmt(p, result, clMid)
  if params.kind != nkEmpty or pragmas.kind != nkEmpty:
    if params.kind == nkEmpty:
      params = newNodeP(nkFormalParams, p)

      params.add(p.emptyNode) # return type

      result[paramsPos] = params

    result[bodyPos] = body
  else:
    body.prefix.add(result.prefix)
    body.mid.add(result.mid)
    body.postfix.add(result.postfix)

    result = body

  setEndInfo()

proc parseProcExpr(p: var Parser, isExpr: bool, kind: TNodeKind): PNode =
  #| routineExpr = ('proc' | 'func' | 'iterator') paramListColon pragma? ('=' COMMENT? stmt)?
  #| routineType = ('proc' | 'iterator') paramListColon pragma?
  # either a proc type or a anonymous proc
  let info = parLineInfo(p)
  let hasSignature = p.tok.tokType in {tkParLe, tkColon} and p.tok.indent < 0
  let skipped = move(p.skipped)
  let params = parseParamList(p)
  let pragmas = optPragmas(p)
  if p.tok.tokType == tkEquals and isExpr:
    getTok(p)
    result = newProcNode(
      kind,
      info,
      body = p.emptyNode,
      params = params,
      name = p.emptyNode,
      pattern = p.emptyNode,
      genericParams = p.emptyNode,
      pragmas = pragmas,
      exceptions = p.emptyNode,
    )
    result.prefix.add skipped
    result[bodyPos] = parseComStmt(p, result, clMid)
  else:
    result = newNodeI(if kind == nkIteratorDef: nkIteratorTy else: nkProcTy, info)
    if hasSignature or pragmas.kind != nkEmpty:
      if hasSignature:
        result.add(params)
      else: # pragmas but no param list, implies typeclass with pragmas
        result.add(p.emptyNode)
      if kind == nkFuncDef:
        parMessage(
          p,
          "func keyword is not allowed in type descriptions, use proc with {.noSideEffect.} pragma instead",
        )
      result.add(pragmas)

  result.prefix.add skipped
  setEndInfo()

proc isExprStart(p: Parser): bool =
  case p.tok.tokType
  of tkSymbol,
      tkAccent,
      tkOpr,
      tkNot,
      tkNil,
      tkCast,
      tkIf,
      tkFor,
      tkProc,
      tkFunc,
      tkIterator,
      tkBind,
      tkBuiltInMagics,
      tkParLe,
      tkBracketLe,
      tkCurlyLe,
      tkIntLit .. tkCustomLit,
      tkVar,
      tkRef,
      tkPtr,
      tkEnum,
      tkTuple,
      tkObject,
      tkWhen,
      tkCase,
      tkOut,
      tkTry,
      tkBlock:
    result = true
  else:
    result = false

proc parseSymbolList(p: var Parser, result: PNode) =
  # progress guaranteed
  while true:
    var s = parseSymbol(p, smAllowNil)
    if s.kind == nkEmpty:
      break
    result.add(s)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, s, clPostfix)
    optInd(p, s)
  setEndInfo()

proc parseTypeDescKAux(p: var Parser, kind: TNodeKind, mode: PrimaryMode): PNode =
  result = newNodeP(kind, p)
  getTok(p)
  if p.tok.indent != -1 and p.tok.indent <= p.currInd:
    return
  optInd(p, result)
  let isTypedef = mode == pmTypeDef and p.tok.tokType in {tkObject, tkTuple}
  if not isOperator(p.tok) and isExprStart(p):
    if isTypedef:
      result.add(parseTypeDefValue(p))
    else:
      result.add(primary(p, mode))
  if kind == nkDistinctTy and p.tok.tokType == tkSymbol:
    # XXX document this feature!
    var nodeKind: TNodeKind
    if p.tok.ident.s == "with":
      nodeKind = nkWith
    elif p.tok.ident.s == "without":
      nodeKind = nkWithout
    else:
      return result
    getTok(p)
    let list = newNodeP(nodeKind, p)
    result.add list
    parseSymbolList(p, list)
  if mode == pmTypeDef and not isTypedef:
    result = parseOperators(p, result, -1, mode)
  setEndInfo()

proc parseVarTuple(p: var Parser): PNode

proc parseFor(p: var Parser): PNode =
  #| forStmt = 'for' ((varTuple / identWithPragma) ^+ comma) 'in' expr colcom stmt
  #| forExpr = forStmt
  getTokNoInd(p)
  result = newNodeP(nkForStmt, p)
  if p.tok.tokType == tkParLe:
    result.add(parseVarTuple(p))
  else:
    var a = identWithPragma(p)
    result.add(a)
    while p.tok.tokType == tkComma:
      getTok(p)
      splitLookahead(p, a, clPostfix)
      optInd(p, a)
      if p.tok.tokType == tkParLe:
        result.add(parseVarTuple(p))
        break
      a = identWithPragma(p)
      result.add(a)
  eat(p, tkIn)
  result.add(parseExpr(p))
  result.add(parseColComStmt(p, result, clMid))
  setEndInfo()

proc parseExpr(p: var Parser): PNode =
  #| expr = (blockExpr
  #|       | ifExpr
  #|       | whenExpr
  #|       | caseStmt
  #|       | forExpr
  #|       | tryExpr)
  #|       / simpleExpr
  case p.tok.tokType
  of tkBlock:
    result = parseBlock(p)
  of tkIf:
    result = parseIfOrWhenExpr(p, nkIfExpr)
  of tkFor:
    result = parseFor(p)
  of tkWhen:
    result = parseIfOrWhenExpr(p, nkWhenExpr)
  of tkCase:
    # Currently we think nimpretty is good enough with case expressions,
    # so it is allowed to touch them:
    #nimprettyDontTouch:
    result = parseCase(p)
  of tkTry:
    result = parseTry(p, isExpr = true)
  else:
    result = simpleExpr(p)
  setEndInfo()

proc parseEnum(p: var Parser): PNode
proc parseObject(p: var Parser): PNode
proc parseTypeClass(p: var Parser): PNode

proc primary(p: var Parser, mode: PrimaryMode): PNode =
  #| simplePrimary = SIGILLIKEOP? identOrLiteral primarySuffix*
  #| commandStart = &('`'|IDENT|literal|'cast'|'addr'|'type'|'var'|'out'|
  #|                  'static'|'enum'|'tuple'|'object'|'proc')
  #| primary = simplePrimary (commandStart expr (doBlock extraPostExprBlock*)?)?
  #|         / operatorB primary
  #|         / routineExpr
  #|         / rawTypeDesc
  #|         / prefixOperator primary
  # XXX strong spaces need to be reflected in commandStart
  # command part is handled in the primarySuffix proc
  # prefix operators:
  if isOperator(p.tok):
    # Note 'sigil like' operators are currently not reflected in the grammar
    # and should be removed for Nim 2.0, I don't think anybody uses them.
    let isSigil = isSigilLike(p.tok)
    result = newNodeP(nkPrefix, p)
    var a = newIdentNodeP(p.tok.ident, p)
    result.add(a)
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)
    const identOrLiteralKinds =
      tkBuiltInMagics + {
        tkSymbol,
        tkAccent,
        tkNil,
        tkIntLit .. tkCustomLit,
        tkCast,
        tkOut,
        tkParLe,
        tkBracketLe,
        tkCurlyLe,
      }
    if isSigil and p.tok.tokType in identOrLiteralKinds:
      let baseInd = p.lex.currLineIndent
      result.add(identOrLiteral(p, mode))
      splitLookahead(p, result[^1], clPostfix)
      result = primarySuffix(p, result, baseInd, mode)
    else:
      result.add(primary(p, pmNormal))
    return

  case p.tok.tokType
  of tkProc:
    getTok(p)
    result = parseProcExpr(p, mode != pmTypeDesc, nkLambda)
  of tkFunc:
    getTok(p)
    result = parseProcExpr(p, mode != pmTypeDesc, nkFuncDef)
  of tkIterator:
    getTok(p)
    result = parseProcExpr(p, mode != pmTypeDesc, nkIteratorDef)
  of tkBind:
    # legacy syntax, no-op in current nim
    result = newNodeP(nkBind, p)
    getTok(p)
    splitLookahead(p, result, clPostfix) # TODO mid
    optInd(p, result)
    result.add(primary(p, pmNormal))
  of tkTuple, tkEnum, tkObject, tkConcept, tkVar, tkOut, tkRef, tkPtr, tkDistinct:
    result = parseTypeDesc(p)
  else:
    let baseInd = p.lex.currLineIndent
    result = identOrLiteral(p, mode)
    # Make sure that primarySuffix has the right token to work with, but don't
    # consume comments just yet (so they can be attached to an appropriate
    # outer node if there is no suffix)
    result = primarySuffix(p, result, baseInd, mode)

proc binaryNot(p: var Parser, a: PNode): PNode =
  if p.tok.tokType == tkNot:
    let notOpr = newIdentNodeP(p.tok.ident, p)
    getTok(p)
    optInd(p, notOpr)
    let b = primary(p, pmTypeDesc)
    result = newNodeP(nkInfix, p)
    result.info = a.info
    result.endInfo = b.endInfo
    result.add notOpr
    result.add a
    result.add b
    setEndInfo(result)
  else:
    result = a

proc parseTypeDesc(p: var Parser, fullExpr = false): PNode =
  #| rawTypeDesc = (tupleType | routineType | 'enum' | 'object' |
  #|                 ('var' | 'out' | 'ref' | 'ptr' | 'distinct') typeDesc?)
  #|                 ('not' primary)?
  #| typeDescExpr = (routineType / simpleExpr) ('not' primary)?
  #| typeDesc = rawTypeDesc / typeDescExpr
  if fullExpr:
    result = simpleExpr(p, pmTypeDesc)
  else:
    case p.tok.tokType
    of tkTuple:
      result = parseTuple(p, false)
    of tkProc:
      getTok(p)
      result = parseProcExpr(p, false, nkLambda)
    of tkIterator:
      getTok(p)
      result = parseProcExpr(p, false, nkIteratorDef)
    of tkEnum:
      result = newNodeP(nkEnumTy, p)
      getTok(p)
    of tkObject:
      result = newNodeP(nkObjectTy, p)
      getTok(p)
    of tkConcept:
      parMessage(p, "the 'concept' keyword is only valid in 'type' sections")
    of tkVar:
      result = parseTypeDescKAux(p, nkVarTy, pmTypeDesc)
    of tkOut:
      result = parseTypeDescKAux(p, nkOutTy, pmTypeDesc)
    of tkRef:
      result = parseTypeDescKAux(p, nkRefTy, pmTypeDesc)
    of tkPtr:
      result = parseTypeDescKAux(p, nkPtrTy, pmTypeDesc)
    of tkDistinct:
      result = parseTypeDescKAux(p, nkDistinctTy, pmTypeDesc)
    else:
      result = simpleExpr(p, pmTypeDesc)
  result = binaryNot(p, result)
  setEndInfo()

proc parseTypeDefValue(p: var Parser): PNode =
  #| typeDefValue = ((tupleDecl | enumDecl | objectDecl | conceptDecl |
  #|                  ('ref' | 'ptr' | 'distinct') (tupleDecl | objectDecl))
  #|                / (simpleExpr (exprEqExpr ^+ comma postExprBlocks?)?))
  #|                ('not' primary)?
  case p.tok.tokType
  of tkTuple:
    result = parseTuple(p, true)
  of tkRef:
    result = parseTypeDescKAux(p, nkRefTy, pmTypeDef)
  of tkPtr:
    result = parseTypeDescKAux(p, nkPtrTy, pmTypeDef)
  of tkDistinct:
    result = parseTypeDescKAux(p, nkDistinctTy, pmTypeDef)
  of tkEnum:
    result = parseEnum(p)
  of tkObject:
    result = parseObject(p)
  of tkConcept:
    result = parseTypeClass(p)
  else:
    result = simpleExpr(p, pmTypeDef)
    if p.tok.tokType != tkNot:
      if result.kind == nkCommand:
        var isFirstParam = false
        while p.tok.tokType == tkComma:
          getTok(p)
          assert result.len > 0
          splitLookahead(p, result[^1], clPostfix)
          optInd(p, result)

          result.add(commandParam(p, isFirstParam, pmTypeDef))
        splitLookahead(p, result[^1], clPostfix)
      result = postExprBlocks(p, result)
  result = binaryNot(p, result)
  setEndInfo()

proc makeCall(n: PNode): PNode =
  ## Creates a call if the given node isn't already a call.
  if n.kind in nkCallKinds:
    n
  else:
    wrap(newNodeI(nkCall, n.info), n)

proc postExprBlocks(p: var Parser, x: PNode): PNode =
  #| extraPostExprBlock = ( IND{=} doBlock
  #|                      | IND{=} 'of' exprList ':' stmt
  #|                      | IND{=} 'elif' expr ':' stmt
  #|                      | IND{=} 'except' optionalExprList ':' stmt
  #|                      | IND{=} 'finally' ':' stmt
  #|                      | IND{=} 'else' ':' stmt )
  #| postExprBlocks = (doBlock / ':' (extraPostExprBlock / stmt)) extraPostExprBlock*
  result = x
  if p.tok.indent >= 0:
    return

  var
    openingParams = p.emptyNode
    openingPragmas = p.emptyNode

  if p.tok.tokType == tkDo:
    getTok(p)
    openingParams = parseParamList(p, retColon = false)
    openingPragmas = optPragmas(p)

  if p.tok.tokType == tkColon:
    result = makeCall(result)
    getTok(p)
    splitLookahead(p, result, clMid)
    if not (p.tok.tokType in {tkOf, tkElif, tkElse, tkExcept, tkFinally} and sameInd(p)):
      var stmtList = newNodeP(nkStmtList, p, withPrefix = false)
      stmtList.add parseStmt(p)
      # to keep backwards compatibility (see tests/vm/tstringnil)
      if stmtList[0].kind == nkStmtList:
        stmtList = stmtList[0]

      stmtList.flags.incl nfBlockArg
      if openingParams.kind != nkEmpty or openingPragmas.kind != nkEmpty:
        if openingParams.kind == nkEmpty:
          openingParams = newNodeP(nkFormalParams, p)
          openingParams.add(p.emptyNode) # return type
        result.add newProcNode(
          nkDo,
          stmtList.info,
          body = stmtList,
          params = openingParams,
          name = p.emptyNode,
          pattern = p.emptyNode,
          genericParams = p.emptyNode,
          pragmas = openingPragmas,
          exceptions = p.emptyNode,
        )
        result[^1].mid = move(result.mid)
      else:
        result.add stmtList

    while sameInd(p):
      var nextBlock: PNode
      let nextToken = p.tok.tokType
      if nextToken == tkDo:
        let info = parLineInfo(p)
        getTok(p)
        nextBlock = parseDoBlock(p, info)
      else:
        case nextToken
        of tkOf:
          nextBlock = newNodeP(nkOfBranch, p)
          exprList(p, tkColon, nextBlock)
        of tkElif:
          nextBlock = newNodeP(nkElifBranch, p)
          getTok(p)
          splitLookahead(p, nextBlock, clMid)
          optInd(p, nextBlock)
          nextBlock.add parseExpr(p)
        of tkExcept:
          nextBlock = newNodeP(nkExceptBranch, p)
          optionalExprList(p, tkColon, nextBlock)
        of tkFinally:
          nextBlock = newNodeP(nkFinally, p)
          getTok(p)
        of tkElse:
          nextBlock = newNodeP(nkElse, p)
          getTok(p)
        else:
          break
        eat(p, tkColon)
        nextBlock.add parseStmt(p)

      nextBlock.flags.incl nfBlockArg
      result.add nextBlock
      if nextBlock.kind in {nkElse, nkFinally}:
        break
  else:
    if openingParams.kind != nkEmpty:
      parMessage(p, "expected ':'")

proc parseExprStmt(p: var Parser): PNode =
  #| exprStmt = simpleExpr postExprBlocks?
  #|          / simplePrimary (exprEqExpr ^+ comma) postExprBlocks?
  #|          / simpleExpr '=' optInd (expr postExprBlocks?)
  var a = simpleExpr(p, pmTrySimple)
  if p.tok.tokType == tkEquals:
    result = newNodeP(nkAsgn, p)
    getTok(p)
    splitLookahead(p, result, clMid)
    optInd(p, result)
    var b = parseExpr(p)
    b = postExprBlocks(p, b)
    result.add(a)
    result.add(b)
  else:
    var isFirstParam = false
    # if an expression is starting here, a simplePrimary was parsed and
    # this is the start of a command
    if p.tok.indent < 0 and isExprStart(p):
      result = wrap(newNodeI(nkCommand, a.info), a)
      let baseIndent = p.currInd
      splitLookahead(p, result[^1], clPostfix)
      while true:
        result.add(commandParam(p, isFirstParam, pmNormal))
        if p.tok.tokType != tkComma or (p.tok.indent >= 0 and p.tok.indent < baseIndent):
          break
        getTok(p)
        splitLookahead(p, result[^1], clPostfix)
        optInd(p, result)
    else:
      result = a
    result = postExprBlocks(p, result)
  setEndInfo()

proc parseModuleName(p: var Parser, kind: TNodeKind): PNode =
  result = parseExpr(p)
  setEndInfo()

proc parseImport(p: var Parser, kind: TNodeKind): PNode =
  #| importStmt = 'import' optInd expr
  #|               ((comma expr)*
  #|               / 'except' optInd (expr ^+ comma))
  #| exportStmt = 'export' optInd expr
  #|               ((comma expr)*
  #|               / 'except' optInd (expr ^+ comma))
  result = newNodeP(kind, p)
  getTok(p) # skip `import` or `export`
  splitLookahead(p, result, clMid)
  optInd(p, result)
  var a = parseModuleName(p, kind)
  result.add(a)
  splitLookahead(p, a, clPostfix)
  if p.tok.tokType in {tkComma, tkExcept}:
    if p.tok.tokType == tkExcept:
      result.transitionSonsKind(succ(kind))
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, result)
    while true:
      # was: while p.tok.tokType notin {tkEof, tkSad, tkDed}:
      p.hasProgress = false
      a = parseModuleName(p, kind)
      if a.kind == nkEmpty or not p.hasProgress:
        break
      splitLookahead(p, a, clPostfix)
      result.add(a)
      if p.tok.tokType != tkComma:
        break
      getTok(p)
      splitLookahead(p, a, clPostfix)
      optInd(p, a)
  #expectNl(p)
  setEndInfo()

proc parseIncludeStmt(p: var Parser): PNode =
  #| includeStmt = 'include' optInd expr ^+ comma
  result = newNodeP(nkIncludeStmt, p)
  getTok(p) # skip `import` or `include`
  splitLookahead(p, result, clMid)
  optInd(p, result)
  while true:
    # was: while p.tok.tokType notin {tkEof, tkSad, tkDed}:
    p.hasProgress = false
    var a = parseExpr(p)
    if a.kind == nkEmpty or not p.hasProgress:
      break
    splitLookahead(p, a, clPostfix)
    result.add(a)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)
  #expectNl(p)
  setEndInfo()

proc parseFromStmt(p: var Parser): PNode =
  #| fromStmt = 'from' expr 'import' optInd expr (comma expr)*
  result = newNodeP(nkFromStmt, p)
  getTok(p) # skip `from`
  optInd(p, result)
  var a = parseModuleName(p, nkImportStmt)
  splitLookahead(p, a, clPrefix) # TODO 'mid' for from?
  result.add(a) #optInd(p, a);
  eat(p, tkImport)
  optInd(p, result)
  while true:
    # p.tok.tokType notin {tkEof, tkSad, tkDed}:
    p.hasProgress = false
    a = parseExpr(p)
    if a.kind == nkEmpty or not p.hasProgress:
      break
    splitLookahead(p, a, clPostfix)
    result.add(a)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)
  #expectNl(p)
  setEndInfo()

proc parseReturnOrRaise(p: var Parser, kind: TNodeKind): PNode =
  #| returnStmt = 'return' optInd expr?
  #| raiseStmt = 'raise' optInd expr?
  #| yieldStmt = 'yield' optInd expr?
  #| discardStmt = 'discard' optInd expr?
  #| breakStmt = 'break' optInd expr?
  #| continueStmt = 'continue' optInd expr?
  result = newNodeP(kind, p)
  getTok(p)
  # We don't split lookahead here because it might be that we end up with no
  # expression - in this case, we don't want the comments attached to
  # the discard midpoint - question is, what to do with them?
  if p.tok.indent >= 0 and p.tok.indent <= p.currInd or not isExprStart(p):
    # NL terminates:
    result.add(p.emptyNode)
  else:
    var e = parseExpr(p)
    splitLookahead(p, e, p.currInd, clPostfix)
    e = postExprBlocks(p, e)
    if e.kind != nkEmpty:
      splitLookahead(p, result, p.currInd, clPostfix)
    result.add(e)
  setEndInfo()

proc parseIfOrWhen(p: var Parser, kind: TNodeKind): PNode =
  #| condStmt = expr colcom stmt COMMENT?
  #|            (IND{=} 'elif' expr colcom stmt)*
  #|            (IND{=} 'else' colcom stmt)?
  #| ifStmt = 'if' condStmt
  #| whenStmt = 'when' condStmt
  result = newNodeP(kind, p)

  while true:
    var branch = newNodeP(nkElifBranch, p)
    getTok(p) # skip `if`, `when`, `elif`
    splitLookahead(p, branch, clMid)
    optInd(p, branch)
    branch.add(parseExpr(p))
    branch.add(parseColComStmt(p, branch, clMid))
    result.add(branch)
    splitLookahead(p, branch, p.currInd, clPostfix)
    if p.tok.tokType != tkElif or not sameOrNoInd(p):
      break
  if p.tok.tokType == tkElse and sameOrNoInd(p):
    var branch = newNodeP(nkElse, p)
    eat(p, tkElse)
    branch.add(parseColComStmt(p, branch, clMid))
    result.add(branch)
  setEndInfo()

proc parseIfOrWhenExpr(p: var Parser, kind: TNodeKind): PNode =
  #| condExpr = expr colcom stmt optInd
  #|         ('elif' expr colcom stmt optInd)*
  #|          'else' colcom stmt
  #| ifExpr = 'if' condExpr
  #| whenExpr = 'when' condExpr
  result = newNodeP(kind, p)
  while true:
    var branch = newNodeP(nkElifExpr, p)
    getTok(p) # skip `if`, `when`, `elif`
    splitLookahead(p, branch, clMid)
    optInd(p, branch)
    branch.add(parseExpr(p))
    branch.add(parseColComStmt(p, branch, clMid))
    result.add(branch)
    splitLookahead(p, branch, clPostfix)
    if p.tok.tokType != tkElif:
      break
  if p.tok.tokType == tkElse:
    var branch = newNodeP(nkElseExpr, p)
    eat(p, tkElse)
    branch.add(parseColComStmt(p, branch, clMid))
    result.add(branch)
  setEndInfo()

proc parseWhile(p: var Parser): PNode =
  #| whileStmt = 'while' expr colcom stmt
  result = newNodeP(nkWhileStmt, p)
  getTok(p)
  splitLookahead(p, result, clMid)
  optInd(p, result)
  result.add(parseExpr(p))
  result.add(parseColComStmt(p, result, clMid))
  setEndInfo()

proc parseCase(p: var Parser): PNode =
  #| ofBranch = 'of' exprList colcom stmt
  #| ofBranches = ofBranch (IND{=} ofBranch)*
  #|                       (IND{=} 'elif' expr colcom stmt)*
  #|                       (IND{=} 'else' colcom stmt)?
  #| caseStmt = 'case' expr ':'? COMMENT?
  #|             (IND{>} ofBranches DED
  #|             | IND{=} ofBranches)
  var
    b: PNode
    inElif = false
    wasIndented = false
  result = newNodeP(nkCaseStmt, p)
  getTok(p)
  result.add(parseExpr(p))
  if p.tok.tokType == tkColon:
    getTok(p)
  splitLookahead(p, result, clMid)

  let oldInd = p.currInd
  if realInd(p):
    p.currInd = p.tok.indent
    wasIndented = true
    splitLookahead(p, result, clMid)

  while sameInd(p):
    case p.tok.tokType
    of tkOf:
      if inElif:
        break
      b = newNodeP(nkOfBranch, p)
      exprList(p, tkColon, b)
    of tkElif:
      inElif = true
      b = newNodeP(nkElifBranch, p)
      getTok(p)
      splitLookahead(p, b, clMid)
      optInd(p, b)
      b.add(parseExpr(p))
    of tkElse:
      b = newNodeP(nkElse, p)
      getTok(p)
      splitLookahead(p, b, clMid)
    else:
      break
    b.add(parseColComStmt(p, b, clMid))
    result.add(b)
    if b.kind == nkElse:
      break

  if wasIndented:
    p.currInd = oldInd
  setEndInfo()

proc parseTry(p: var Parser, isExpr: bool): PNode =
  #| tryStmt = 'try' colcom stmt &(IND{=}? 'except'|'finally')
  #|            (IND{=}? 'except' optionalExprList colcom stmt)*
  #|            (IND{=}? 'finally' colcom stmt)?
  #| tryExpr = 'try' colcom stmt &(optInd 'except'|'finally')
  #|            (optInd 'except' optionalExprList colcom stmt)*
  #|            (optInd 'finally' colcom stmt)?
  result = newNodeP(nkTryStmt, p)
  let parentIndent = p.currInd # isExpr
  getTok(p)
  result.add(parseColComStmt(p, result, clMid))

  var b: PNode = nil
  while sameOrNoInd(p) or (isExpr and parentIndent <= p.tok.indent):
    case p.tok.tokType
    of tkExcept:
      b = newNodeP(nkExceptBranch, p)
      optionalExprList(p, tkColon, b)
    of tkFinally:
      b = newNodeP(nkFinally, p)
      getTok(p)
    else:
      break
    b.add(parseColComStmt(p, b, clMid))
    result.add(b)
  if b == nil:
    parMessage(p, "expected 'except'")
  setEndInfo()

proc parseExceptBlock(p: var Parser, kind: TNodeKind): PNode =
  result = newNodeP(kind, p)
  getTok(p)
  result.add(parseColComStmt(p, result, clMid))
  setEndInfo()

proc parseBlock(p: var Parser): PNode =
  #| blockStmt = 'block' symbol? colcom stmt
  #| blockExpr = 'block' symbol? colcom stmt
  result = newNodeP(nkBlockStmt, p)
  getTokNoInd(p)
  if p.tok.tokType == tkColon:
    result.add(p.emptyNode)
  else:
    result.add(parseSymbol(p))
  result.add(parseColComStmt(p, result, clMid))
  setEndInfo()

proc parseStaticOrDefer(p: var Parser, k: TNodeKind): PNode =
  #| staticStmt = 'static' colcom stmt
  #| deferStmt = 'defer' colcom stmt
  result = newNodeP(k, p)
  getTok(p)
  result.add(parseColComStmt(p, result, clMid))
  setEndInfo()

proc parseAsm(p: var Parser): PNode =
  #| asmStmt = 'asm' pragma? (STR_LIT | RSTR_LIT | TRIPLESTR_LIT)
  result = newNodeP(nkAsmStmt, p)
  getTokNoInd(p)
  if p.tok.tokType == tkCurlyDotLe:
    result.add(parsePragma(p))
  else:
    result.add(p.emptyNode)
  case p.tok.tokType
  of tkStrLit:
    result.add(newStrNodeP(nkStrLit, p.tok.literal, p))
  of tkRStrLit:
    result.add(newStrNodeP(nkRStrLit, p.tok.literal, p))
  of tkTripleStrLit:
    result.add(newStrNodeP(nkTripleStrLit, p.tok.literal, p))
  else:
    parMessage(p, "the 'asm' statement takes a string literal")
    result.add(p.emptyNode)
    return
  getTok(p)
  setEndInfo()

proc parseGenericParam(p: var Parser): PNode =
  #| genericParam = symbol (comma symbol)* (colon expr)? ('=' optInd expr)?
  var a: PNode
  result = newNodeP(nkIdentDefs, p)
  # progress guaranteed
  while true:
    case p.tok.tokType
    of tkIn, tkOut:
      let x = p.lex.cache.getIdent(if p.tok.tokType == tkIn: "in" else: "out")
      a = newNodeP(nkPrefix, p)
      a.add newIdentNodeP(x, p)
      getTok(p)
      expectIdent(p)
      a.add(parseSymbol(p))
    of tkSymbol, tkAccent:
      a = parseSymbol(p)
      if a.kind == nkEmpty:
        return
    else:
      break
    result.add(a)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)
  if p.tok.tokType == tkColon:
    getTok(p)
    splitLookahead(p, result, clPostfix) # TODO mid?
    optInd(p, result)
    result.add(parseExpr(p))
  else:
    result.add(p.emptyNode)
  if p.tok.tokType == tkEquals:
    getTok(p)
    splitLookahead(p, result, clPostfix) # TODO mid?
    optInd(p, result)
    result.add(parseExpr(p))
  else:
    result.add(p.emptyNode)
  setEndInfo()

proc parseGenericParamList(p: var Parser): PNode =
  #| genericParamList = '[' optInd
  #|   genericParam ^* (comma/semicolon) optPar ']'
  result = newNodeP(nkGenericParams, p)
  getTok(p)
  optInd(p, result)
  # progress guaranteed
  while p.tok.tokType in {tkSymbol, tkAccent, tkIn, tkOut}:
    var a = parseGenericParam(p)
    result.add(a)
    if p.tok.tokType notin {tkComma, tkSemiColon}:
      # All comments before closing token
      a.postfix.add move(p.skipped)
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
  optPar(p)
  eat(p, tkBracketRi)
  setEndInfo()

proc parsePattern(p: var Parser): PNode =
  #| pattern = '{' stmt '}'
  eat(p, tkCurlyLe)
  result = parseStmt(p)
  eat(p, tkCurlyRi)
  setEndInfo()

proc parseRoutine(p: var Parser, kind: TNodeKind): PNode =
  #| indAndComment = (IND{>} COMMENT)? | COMMENT?
  #| routine = optInd identVis pattern? genericParamList?
  #|   paramListColon pragma? ('=' COMMENT? stmt)? indAndComment
  result = newNodeP(kind, p)
  getTok(p)
  splitLookahead(p, result, clMid)
  optInd(p, result)
  if kind in {nkProcDef, nkLambda, nkIteratorDef, nkFuncDef} and
      p.tok.tokType notin {tkSymbol, tokKeywordLow .. tokKeywordHigh, tkAccent}:
    # no name; lambda or proc type
    # in every context that we can parse a routine, we can also parse these
    result = parseProcExpr(p, true, if kind == nkProcDef: nkLambda else: kind)
    return
  result.add(identVis(p))
  if p.tok.tokType == tkCurlyLe and p.validInd:
    result.add(p.parsePattern)
  else:
    result.add(p.emptyNode)
  if p.tok.tokType == tkBracketLe and p.validInd:
    result.add(p.parseGenericParamList)
  else:
    result.add(p.emptyNode)
  result.add(p.parseParamList)
  if p.tok.tokType == tkCurlyDotLe and p.validInd:
    result.add(p.parsePragma)
  else:
    result.add(p.emptyNode)
  # empty exception tracking:
  result.add(p.emptyNode)
  let maybeMissEquals = p.tok.tokType != tkEquals
  if (not maybeMissEquals) and p.validInd:
    getTok(p)
    splitLookahead(p, result, clMid)
    result.add(parseStmt(p, result.mid.len > 0))
  else:
    result.add(p.emptyNode)
  indAndComment(p, result, maybeMissEquals = maybeMissEquals) # no comments really

  setEndInfo()

proc parseCommentStmt(p: var Parser): PNode =
  #| commentStmt = COMMENT
  result = newNodeP(nkCommentStmt, p)
  result.strVal = p.tok.literal
  setEndInfo()
  getTok(p)

proc parseSection(
    p: var Parser, result: PNode, defparser: proc(p: var Parser): PNode {.nimcall.}
) =
  #| section(RULE) = COMMENT? RULE / (IND{>} (RULE / COMMENT)^+IND{=} DED)
  if result.kind != nkTypeSection:
    getTok(p)
  splitLookahead(p, result, clMid)
  if realInd(p):
    withInd(p):
      while sameInd(p) or p.tok.tokType == tkComment and validInd(p):
        case p.tok.tokType
        of tkSymbol, tkAccent, tkParLe:
          var a = defparser(p)
          splitLookahead(p, a, p.currInd, clPostfix)
          result.add(a)
        of tkComment:
          var a = parseCommentStmt(p)
          result.add(a)
        else:
          parMessage(p, errIdentifierExpected, p.tok)
          break
      addSkipped(p, result)
    if result.len == 0:
      parMessage(p, errIdentifierExpected, p.tok)
  elif p.tok.tokType in {tkSymbol, tkAccent, tkParLe} and p.tok.indent < 0:
    # tkParLe is allowed for ``var (x, y) = ...`` tuple parsing
    result.add(defparser(p))
    splitLookahead(p, result[^1], p.currInd, clPostfix)
  else:
    parMessage(p, errIdentifierExpected, p.tok)
  setEndInfo()

proc parseEnum(p: var Parser): PNode =
  #| enumDecl = 'enum' optInd (symbol pragma? optInd ('=' optInd expr COMMENT?)? comma?)+
  result = newNodeP(nkEnumTy, p)
  getTok(p)
  result.add(p.emptyNode)
  optInd(p, result)
  splitLookahead(p, result, clMid)
  # progress guaranteed
  while true:
    let symInd = if p.tok.indent == -1: p.currInd else: p.tok.indent
    var a = parseSymbol(p)
    if a.kind == nkEmpty:
      return

    var symPragma = a
    if (p.tok.indent < 0 or p.tok.indent >= p.currInd) and p.tok.tokType == tkCurlyDotLe:
      let pragma = optPragmas(p)
      symPragma = newNodeP(nkPragmaExpr, p, withPrefix = false)
      symPragma.add(a)
      symPragma.add(pragma)
    if p.tok.indent >= 0 and p.tok.indent <= p.currInd:
      splitLookahead(p, symPragma, symInd, clPostfix)
      result.add(symPragma)
      break

    if p.tok.tokType == tkEquals and p.tok.indent < 0:
      getTok(p)
      optInd(p, symPragma)
      var b = symPragma
      symPragma = newNodeP(nkEnumFieldDef, p)
      symPragma.add(b)
      symPragma.add(parseExpr(p))
    if p.tok.tokType == tkComma and p.tok.indent < 0:
      getTok(p)
    splitLookahead(p, symPragma, symInd, clPostfix)
    result.add(symPragma)
    if p.tok.indent >= 0 and p.tok.indent <= p.currInd or p.tok.tokType == tkEof:
      break
  if result.len <= 1:
    parMessage(p, errIdentifierExpected, p.tok)
  setEndInfo()

proc parseObjectPart(p: var Parser): PNode
proc parseObjectWhen(p: var Parser): PNode =
  #| objectWhen = 'when' expr colcom objectPart COMMENT?
  #|             ('elif' expr colcom objectPart COMMENT?)*
  #|             ('else' colcom objectPart COMMENT?)?
  result = newNodeP(nkRecWhen, p)
  # progress guaranteed
  while sameInd(p):
    getTok(p) # skip `when`, `elif`
    var branch = newNodeP(nkElifBranch, p)
    splitLookahead(p, branch, clMid)
    optInd(p, branch)
    branch.add(parseExpr(p))
    eat(p, tkColon)
    splitLookahead(p, branch, clMid)
    branch.add(parseObjectPart(p))
    result.add(branch)
    if p.tok.tokType != tkElif:
      break
  if p.tok.tokType == tkElse and sameInd(p):
    var branch = newNodeP(nkElse, p)
    eat(p, tkElse)
    eat(p, tkColon)
    splitLookahead(p, branch, clMid)
    optInd(p, result)
    branch.add(parseObjectPart(p))
    result.add(branch)
  setEndInfo()

proc parseObjectCase(p: var Parser): PNode =
  #| objectBranch = 'of' exprList colcom objectPart
  #| objectBranches = objectBranch (IND{=} objectBranch)*
  #|                       (IND{=} 'elif' expr colcom objectPart)*
  #|                       (IND{=} 'else' colcom objectPart)?
  #| objectCase = 'case' declColonEquals ':'? COMMENT?
  #|             (IND{>} objectBranches DED
  #|             | IND{=} objectBranches)
  result = newNodeP(nkRecCase, p)
  splitLookahead(p, result, clMid)
  getTokNoInd(p)
  var a = parseIdentColonEquals(p, {withPragma})
  splitLookahead(p, result, clMid)
  result.add(a)
  if p.tok.tokType == tkColon:
    getTok(p)
    splitLookahead(p, result, clMid)
  var wasIndented = false
  let oldInd = p.currInd
  if realInd(p):
    p.currInd = p.tok.indent
    wasIndented = true
  # progress guaranteed
  while sameInd(p):
    var b: PNode
    case p.tok.tokType
    of tkOf:
      b = newNodeP(nkOfBranch, p)
      exprList(p, tkColon, b)
    of tkElse:
      b = newNodeP(nkElse, p)
      getTok(p)
    else:
      break
    eat(p, tkColon)
    splitLookahead(p, b, clMid)
    var fields = parseObjectPart(p)
    if fields.kind == nkEmpty:
      parMessage(p, errIdentifierExpected, p.tok)
      fields = newNodeP(nkNilLit, p) # don't break further semantic checking
    b.add(fields)
    result.add(b)
    if b.kind == nkElse:
      break
  if wasIndented:
    p.currInd = oldInd
  setEndInfo()

proc parseObjectPart(p: var Parser): PNode =
  #| objectPart = IND{>} objectPart^+IND{=} DED
  #|            / objectWhen / objectCase / 'nil' / 'discard' / declColonEquals
  if realInd(p):
    result = newNodeP(nkRecList, p, withPrefix = false)
    withInd(p):
      while sameInd(p):
        case p.tok.tokType
        of tkCase, tkWhen, tkSymbol, tkAccent, tkNil, tkDiscard:
          result.add(parseObjectPart(p))
          splitLookahead(p, result[^1], p.currInd, clPostfix)
        else:
          parMessage(p, errIdentifierExpected, p.tok)
          break
  elif sameOrNoInd(p):
    case p.tok.tokType
    of tkWhen:
      result = parseObjectWhen(p)
    of tkCase:
      result = parseObjectCase(p)
    of tkSymbol, tkAccent:
      result = parseIdentColonEquals(p, {withPragma})
    of tkNil, tkDiscard:
      result = newNodeP(nkNilLit, p)
      getTok(p)
      splitLookahead(p, result, clPostfix)
    else:
      result = p.emptyNode
  else:
    result = p.emptyNode
  setEndInfo()

proc parseObject(p: var Parser): PNode =
  #| objectDecl = 'object' ('of' typeDesc)? COMMENT? objectPart
  result = newNodeP(nkObjectTy, p)
  getTok(p)
  result.add(p.emptyNode) # compatibility with old pragma node
  if p.tok.tokType == tkOf and p.tok.indent < 0:
    var a = newNodeP(nkOfInherit, p)
    getTok(p)
    a.add(parseTypeDesc(p))
    result.add(a)
  else:
    result.add(p.emptyNode)
  splitLookahead(p, result, clMid)
  # an initial IND{>} HAS to follow:
  if not realInd(p):
    result.add(p.emptyNode)
  else:
    # Any comments just after `object` are assumed to be part of the object,
    # not the fields that follow
    splitLookahead(p, result, p.tok.indent - 1, clMid)
    result.add(parseObjectPart(p))
  setEndInfo()

proc parseTypeClassParam(p: var Parser): PNode =
  let modifier =
    case p.tok.tokType
    of tkOut, tkVar: nkVarTy
    of tkPtr: nkPtrTy
    of tkRef: nkRefTy
    of tkStatic: nkStaticTy
    of tkType: nkTypeOfExpr
    else: nkEmpty

  if modifier != nkEmpty:
    result = newNodeP(modifier, p)
    getTok(p)
    result.add(p.parseSymbol)
  else:
    result = p.parseSymbol
  setEndInfo()

proc parseTypeClass(p: var Parser): PNode =
  #| conceptParam = ('var' | 'out')? symbol
  #| conceptDecl = 'concept' conceptParam ^* ',' (pragma)? ('of' typeDesc ^* ',')?
  #|               &IND{>} stmt
  result = newNodeP(nkTypeClassTy, p)
  getTok(p)

  if p.tok.indent < 0:
    var args = newNodeP(nkArgList, p)
    result.add(args)
    args.add(p.parseTypeClassParam)
    while p.tok.tokType == tkComma:
      getTok(p)
      args.add(p.parseTypeClassParam)
  else:
    result.add(p.emptyNode) # see ast.isNewStyleConcept
  if p.tok.tokType == tkCurlyDotLe and p.validInd:
    result.add(parsePragma(p))
  else:
    result.add(p.emptyNode)
  if p.tok.tokType == tkOf and p.tok.indent < 0:
    var a = newNodeP(nkOfInherit, p)
    getTok(p)
    # progress guaranteed
    while true:
      a.add(parseTypeDesc(p))
      if p.tok.tokType != tkComma:
        break
      getTok(p)
    result.add(a)
  else:
    result.add(p.emptyNode)
  # an initial IND{>} HAS to follow:
  if not realInd(p):
    if result.isNewStyleConcept:
      parMessage(
        p,
        "routine expected, but found '$1' (empty new-styled concepts are not allowed)",
        p.tok,
      )
    result.add(p.emptyNode)
  else:
    result.add(parseStmt(p))
  setEndInfo()

proc parseTypeDef(p: var Parser): PNode =
  #|
  #| typeDef = identVisDot genericParamList? pragma '=' optInd typeDefValue
  #|             indAndComment?
  result = newNodeP(nkTypeDef, p)
  var identifier = identVis(p, allowDot = true)
  var identPragma = identifier
  var pragma: PNode
  var genericParam: PNode
  var next = identifier

  if p.tok.tokType == tkBracketLe and p.validInd:
    splitLookahead(p, identifier, clPostfix)
    genericParam = parseGenericParamList(p)
    next = genericParam
  else:
    genericParam = p.emptyNode
  # pragma = optPragmas(p)
  if p.tok.tokType == tkCurlyDotLe and (p.tok.indent < 0 or realInd(p)):
    splitLookahead(p, next, clPostfix)
    pragma = parsePragma(p)
    next = pragma
  else:
    pragma = p.emptyNode
  if pragma.kind != nkEmpty:
    identPragma = newNodeP(nkPragmaExpr, p, withPrefix = false)
    identPragma.add(identifier)
    identPragma.add(pragma)

  result.add(identPragma)
  result.add(genericParam)
  if p.tok.tokType == tkEquals:
    result.info = parLineInfo(p)
    getTok(p)
    splitLookahead(p, result, clMid)
    optInd(p, result)
    result.add(parseTypeDefValue(p))
  else:
    result.add(p.emptyNode)
  setEndInfo()

proc parseVarTuple(p: var Parser): PNode =
  #| varTupleLhs = '(' optInd (identWithPragma / varTupleLhs) ^+ comma optPar ')'
  #| varTuple = varTupleLhs '=' optInd expr
  result = newNodeP(nkVarTuple, p)
  getTok(p) # skip '('
  optInd(p, result)
  # progress guaranteed
  while p.tok.tokType in {tkSymbol, tkAccent, tkParLe}:
    var a: PNode
    if p.tok.tokType == tkParLe:
      a = parseVarTuple(p)
      a.add(p.emptyNode)
    else:
      a = identWithPragma(p, allowDot = true)
    result.add(a)
    if p.tok.tokType != tkComma:
      # All comments before closing token
      a.postfix.add move(p.skipped)
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
  result.add(p.emptyNode) # no type desc
  optPar(p)
  eat(p, tkParRi)
  setEndInfo()

proc parseVariable(p: var Parser): PNode =
  #| colonBody = colcom stmt postExprBlocks?
  #| variable = (varTuple / identColonEquals) colonBody? indAndComment
  if p.tok.tokType == tkParLe:
    result = parseVarTuple(p)
    eat(p, tkEquals)
    splitLookahead(p, result, clMid)
    optInd(p, result)
    result.add(parseExpr(p))
    splitLookahead(p, result[^1], clPostfix)
  else:
    result = parseIdentColonEquals(p, {withPragma, withDot})
  result[^1] = postExprBlocks(p, result[^1])
  setEndInfo()

proc parseConstant(p: var Parser): PNode =
  #| constant = (varTuple / identWithPragma) (colon typeDesc)? '=' optInd expr indAndComment
  if p.tok.tokType == tkParLe:
    result = parseVarTuple(p)
  else:
    result = newNodeP(nkConstDef, p)

    # We let comments sit as prefixes to whatever comes next - this works well
    # with tkColon / tkEquals which don't have indent requirements..
    result.add(identWithPragma(p))
    if p.tok.tokType == tkColon:
      getTok(p)
      optInd(p, result)
      result.add(parseTypeDesc(p))
    else:
      result.add(p.emptyNode)
  eat(p, tkEquals)
  splitLookahead(p, result, clMid)
  optInd(p, result)
  result.add(parseExpr(p))
  result[^1] = postExprBlocks(p, result[^1])
  setEndInfo()

proc parseBind(p: var Parser, k: TNodeKind): PNode =
  #| bindStmt = 'bind' optInd qualifiedIdent ^+ comma
  #| mixinStmt = 'mixin' optInd qualifiedIdent ^+ comma
  result = newNodeP(k, p)
  getTok(p)
  optInd(p, result) # TODO mid?
  # progress guaranteed
  while true:
    var a = qualifiedIdent(p)
    result.add(a)
    if p.tok.tokType != tkComma:
      break
    getTok(p)
    splitLookahead(p, a, clPostfix)
    optInd(p, a)
  #expectNl(p)
  setEndInfo()

proc parseStmtPragma(p: var Parser): PNode =
  #| pragmaStmt = pragma (':' COMMENT? stmt)?
  result = parsePragma(p)
  if p.tok.tokType == tkColon and p.tok.indent < 0:
    let a = result
    result = newNodeI(nkPragmaBlock, a.info)
    result.add a
    getTok(p)
    splitLookahead(p, result, clMid)
    result.add parseStmt(p)
    splitLookahead(p, result, clPostfix)
  setEndInfo()

proc simpleStmt(p: var Parser): PNode =
  #| simpleStmt = ((returnStmt | raiseStmt | yieldStmt | discardStmt | breakStmt
  #|            | continueStmt | pragmaStmt | importStmt | exportStmt | fromStmt
  #|            | includeStmt | commentStmt) / exprStmt) COMMENT?
  #|
  case p.tok.tokType
  of tkReturn:
    result = parseReturnOrRaise(p, nkReturnStmt)
  of tkRaise:
    result = parseReturnOrRaise(p, nkRaiseStmt)
  of tkYield:
    result = parseReturnOrRaise(p, nkYieldStmt)
  of tkDiscard:
    result = parseReturnOrRaise(p, nkDiscardStmt)
  of tkBreak:
    result = parseReturnOrRaise(p, nkBreakStmt)
  of tkContinue:
    result = parseReturnOrRaise(p, nkContinueStmt)
  of tkCurlyDotLe:
    result = parseStmtPragma(p)
  of tkImport:
    result = parseImport(p, nkImportStmt)
  of tkExport:
    result = parseImport(p, nkExportStmt)
  of tkFrom:
    result = parseFromStmt(p)
  of tkInclude:
    result = parseIncludeStmt(p)
  of tkComment:
    result = parseCommentStmt(p)
  else:
    if isExprStart(p):
      result = parseExprStmt(p)
    else:
      result = p.emptyNode

proc complexOrSimpleStmt(p: var Parser): PNode =
  #| complexOrSimpleStmt = (ifStmt | whenStmt | whileStmt
  #|                     | tryStmt | forStmt
  #|                     | blockStmt | staticStmt | deferStmt | asmStmt
  #|                     | 'proc' routine
  #|                     | 'method' routine
  #|                     | 'func' routine
  #|                     | 'iterator' routine
  #|                     | 'macro' routine
  #|                     | 'template' routine
  #|                     | 'converter' routine
  #|                     | 'type' section(typeDef)
  #|                     | 'const' section(constant)
  #|                     | ('let' | 'var' | 'using') section(variable)
  #|                     | bindStmt | mixinStmt)
  #|                     / simpleStmt
  case p.tok.tokType
  of tkIf:
    result = parseIfOrWhen(p, nkIfStmt)
  of tkWhile:
    result = parseWhile(p)
  of tkCase:
    result = parseCase(p)
  of tkTry:
    result = parseTry(p, isExpr = false)
  of tkFinally:
    result = parseExceptBlock(p, nkFinally)
  of tkExcept:
    result = parseExceptBlock(p, nkExceptBranch)
  of tkFor:
    result = parseFor(p)
  of tkBlock:
    result = parseBlock(p)
  of tkStatic:
    result = parseStaticOrDefer(p, nkStaticStmt)
  of tkDefer:
    result = parseStaticOrDefer(p, nkDefer)
  of tkAsm:
    result = parseAsm(p)
  of tkProc:
    result = parseRoutine(p, nkProcDef)
  of tkFunc:
    result = parseRoutine(p, nkFuncDef)
  of tkMethod:
    result = parseRoutine(p, nkMethodDef)
  of tkIterator:
    result = parseRoutine(p, nkIteratorDef)
  of tkMacro:
    result = parseRoutine(p, nkMacroDef)
  of tkTemplate:
    result = parseRoutine(p, nkTemplateDef)
  of tkConverter:
    result = parseRoutine(p, nkConverterDef)
  of tkType:
    let info = parLineInfo(p)
    getTok(p)
    if p.tok.tokType == tkParLe:
      getTok(p)
      result = newNodeI(nkTypeOfExpr, info)
      result.prefix = move(p.skipped)
      result.add(primary(p, pmTypeDesc))
      eat(p, tkParRi)
      result = parseOperators(p, result, -1, pmNormal)
    else:
      result = newNodeI(nkTypeSection, info)
      parseSection(p, result, parseTypeDef)
  of tkConst:
    result = newNodeP(nkConstSection, p, withPrefix = false)
    parseSection(p, result, parseConstant)
  of tkLet:
    result = newNodeP(nkLetSection, p, withPrefix = false)
    parseSection(p, result, parseVariable)
  of tkVar:
    result = newNodeP(nkVarSection, p, withPrefix = false)
    parseSection(p, result, parseVariable)
  of tkWhen:
    result = parseIfOrWhen(p, nkWhenStmt)
  of tkBind:
    result = parseBind(p, nkBindStmt)
  of tkMixin:
    result = parseBind(p, nkMixinStmt)
  of tkUsing:
    result = newNodeP(nkUsingStmt, p, withPrefix = false)
    parseSection(p, result, parseVariable)
  else:
    result = simpleStmt(p)

proc parseStmt(p: var Parser, allowEmpty: bool): PNode =
  #| stmt = (IND{>} complexOrSimpleStmt^+(IND{=} / ';') DED)
  #|      / simpleStmt ^+ ';'
  let nextInd =
    if p.tok.indent < p.currInd and p.skipped.len > 0 and p.skipped[0].indent > p.currInd:
      p.skipped[0].indent
    else:
      p.tok.indent
  if nextInd > p.currInd:
    result = newNodeP(nkStmtList, p, withPrefix = false)
    withInd(p, nextInd):
      addSkipped(p, result)
      splitLookahead(p, result, clPrefix)
      while true:
        if p.tok.indent == p.currInd:
          discard
        elif p.tok.tokType == tkSemiColon:
          getTok(p)
          if p.tok.indent < 0 or p.tok.indent == p.currInd:
            discard
          else:
            break
        else:
          if p.tok.indent > p.currInd and p.tok.tokType notin {tkDot, tkComment}:
            printTok(p.lex.config, p.tok)
            parMessage(p, errInvalidIndentation % "parseStmt")
          addSkipped(p, result)
          break
        if p.tok.tokType in {tkCurlyRi, tkParRi, tkCurlyDotRi, tkBracketRi}:
          # XXX this ensures tnamedparamanonproc still compiles;
          # deprecate this syntax later
          break
        p.hasProgress = false
        if p.tok.tokType in {tkElse, tkElif}:
          break # Allow this too, see tests/parser/tifexprs

        let a = complexOrSimpleStmt(p)
        if a.kind == nkEmpty and not p.hasProgress:
          parMessage(p, errExprExpected & " (parseStmt >)", p.tok)
          break
        else:
          result.add a

        splitLookahead(p, a, p.currInd, clPostfix)
        addSkipped(p, result)
        if not p.hasProgress and p.tok.tokType == tkEof:
          break
  else:
    # the case statement is only needed for better error messages:
    case p.tok.tokType
    of tkIf, tkWhile, tkCase, tkTry, tkFor, tkBlock, tkAsm, tkProc, tkFunc, tkIterator,
        tkMacro, tkType, tkConst, tkWhen, tkVar:
      if allowEmpty:
        result = newNodeP(nkStmtList, p, withPrefix = false)
        addSkipped(p, result)
      else:
        parMessage(p, "nestable statement requires indentation")
        result = p.emptyNode
    else:
      if p.inSemiStmtList > 0:
        result = simpleStmt(p)
        if result.kind == nkEmpty:
          parMessage(p, errExprExpected & " (parseStmt semi)", p.tok)
      else:
        result = newNodeP(nkStmtList, p, withPrefix = false)
        addSkipped(p, result)
        while true:
          if allowEmpty and p.tok.indent >= 0 and p.tok.indent <= p.currInd:
            break
          # if p.tok.indent >= 0:
          #  parMessage(p, errInvalidIndentation % "parseStmt 2")
          p.hasProgress = false
          let a = simpleStmt(p)
          let err = not p.hasProgress
          if a.kind == nkEmpty:
            parMessage(p, errExprExpected & " (parseStmt =)", p.tok)
          result.add(a)
          if p.tok.tokType != tkSemiColon:
            break
          getTok(p)
          if err and p.tok.tokType == tkEof:
            break
  setEndInfo()

proc parseTopLevelStmt(p: var Parser): PNode =
  ## Implements an iterator which, when called repeatedly, returns the next
  ## top-level statement or emptyNode if end of stream.
  result = p.emptyNode
  # progress guaranteed
  while true:
    if p.tok.indent != 0:
      if p.firstTok and p.tok.indent < 0:
        discard
      elif p.tok.tokType != tkSemiColon:
        # special casing for better error messages:
        if p.tok.tokType == tkOpr and p.tok.ident.s == "*":
          parMessage(
            p, errGenerated,
            "invalid indentation; an export marker '*' follows the declared identifier",
          )
        else:
          parMessage(p, errInvalidIndentation & " parseTopLevelStmt 0")
    p.firstTok = false
    case p.tok.tokType
    of tkSemiColon:
      getTok(p)
      if p.tok.indent <= 0:
        discard
      else:
        parMessage(p, errInvalidIndentation & " parseTopLevelStmt 1")
      p.firstTok = true
    of tkEof:
      break
    else:
      result = complexOrSimpleStmt(p)
      if result.kind == nkEmpty:
        parMessage(p, errExprExpected & " parseTopLevelStmt", p.tok)
      else:
        splitLookahead(p, result, 0, clPostfix)
      break
  setEndInfo()

proc parseAll(p: var Parser): PNode =
  ## Parses the rest of the input stream held by the parser into a PNode.
  result = newNodeP(nkStmtList, p, withPrefix = false)
  while true:
    addSkipped(p, result)
    let n = parseTopLevelStmt(p)
    if n.kind == nkEmpty:
      break
    result.add(n)

  addSkipped(p, result)

  setEndInfo()

proc checkFirstLineIndentation*(p: var Parser) =
  if p.tok.indent != 0 and tsLeading in p.tok.spacing:
    parMessage(p, errInvalidIndentation % "checkFirstLineIndentation")

proc parseString*(
    s: string,
    cache: IdentCache,
    config: ConfigRef,
    filename: string = "",
    line: int = 0,
    errorHandler: ErrorHandler = nil,
    printTokens = false,
): PNode =
  ## Parses a string into an AST, returning the top node.
  ## `filename` and `line`, although optional, provide info so that the
  ## compiler can generate correct error messages referring to the original
  ## source.
  var stream = llStreamOpen(s)
  stream.lineOffset = line

  var p: Parser
  p.lex.errorHandler = errorHandler

  openParser(p, AbsoluteFile filename, stream, cache, config, printTokens)
  let fid = config.fileInfoIdx(AbsoluteFile filename)
  config.m.fileInfos[fid.int].fullContent = s
  result = p.parseAll
  closeParser(p)
  setEndInfo()
