#
#

# Some commentary

## A doc comment
##
##

## More doc comments
##

#
# Comment
#

#[ a multiline comment
this is also part of it
and this
]#

##[
  these also come in doc variants
]##

#[
  #[
    they can be nested
  ]#
]#

# Test block comment followed by single-line comment on same line
#[ block comment ]# # trailing comment
##[ doc block comment ]## # trailing doc comment

# Test with actual multiline block comments
#[
  multiline
  block comment
]# # trailing comment should stay on same line

##[
  multiline doc
  block comment
]## # trailing doc comment on same line

template x() =
  ## A template doc comment
  try:
    discard
  except Exception:
    mixin `$` # A comment after mixin
    echo 4

type
  # comment after type keyword
  LonelyObject* = object

type
  SingleValueSetting* {.pure.} = enum
    ## \
    ## settings resulting in a single string value
    arguments ## experimental: the arguments passed after '-r'
    backend
      ## the backend (eg: c|cpp|objc|js); both `nim doc --backend:js`
      ## and `nim js` would imply backend=js
    gc {.deprecated.} ## gc selected
    mm ## memory management selected

  FileSeekPos* = enum
    fspEnd ## Seek relative to end
    # text file handling:

  ## Position relative to which seek should happen.
  # The values are ordered so that they match with stdio
  # SEEK_SET, SEEK_CUR and SEEK_END respectively.
  Object = object
    # comment
    ## more comment
    field: int # Field comment
    # comment between fields
    field2: int ## Field comment again
    fiiiiiiiiiiiiiiiiiiiiiiiieeeeeeeeeld: int
      # loooooooooooooooooooong comment past the max line length

    docfield, ## Doc comment after comma
      docfield2, ## Doc comment again
        ## Multiline
      docfield3: int ## here came the type

  # and here
  NewlineObject = object
    field: int ## doc comment after field
    ## doc comment continues, though not as a postfix - empty line after
    field2: int ## just a doc again

  Inherited = object of RootObj
    # inherited eol comment
    # inherited next line indent comments
    f: int

  CaseObject = object # caseobj eol
    case k: bool # casetype eol
    of true, false: # of eol
      v: string # case field eol

  SomeAlias* = int
    ## alias eol
    ## alias next

  SomeAlias2 {.nodecl.} = # after pragma
    int ## alias2 eol

  SomeAlias3 # alias after symbol
    [T] = # alias after equals
      int # alias after type

  SomeAlias4 = SomeAlias3[int]
    ## after alias4
    ## more after alias4

  SomeAlias5 = ## doc comment after equals before proc
    ## more than one line
    proc(v: int)

  SomeAlias6 = ##
    ## doc comment after equals before proc continued
    proc(v: int)

  ## Some comment before whenobj
  WhenObject = object # whenobject object line
    when false: # when object false line
      discard

  NoField0* = object of RootObj
    ## comment eol
    ## comment nl

  NoField1* = object of RootObj
    ## comment nofield1 eol
    ## comment nl

  CommentedTuple* =
    tuple
      ## Comment here
      field: int ## comment tuple field
      field2: int ## comment tuple field2

  CommentedTuple2* =
    tuple
      ## comment tuple only mid
      field: int

when defined(somecond): # when colon line
  # when first line
  discard
# comment
else: # else colon line
  # else first line
  discard

if true:
  ## doc-comment-only if

block:
  if true:
    ## doc-comment-only if nested

while false:
  ## doc-comment-only while

if true:
  # if next line
  discard
else:
  # else next line
  discard

if true: # if colon line
  discard
else: # else colon line
  discard

if true:
  # if dedented colon line
  discard
# before else dedented
else: # else colon line
  discard

# before module
import module # with a comment
import module ## with a comment

try: # try colon line
  # try first line
  discard
  # try last line
except: # except colon line
  # except first line
  discard
  # except last line
finally: # Finally colon line
  # finally first line
  discard
  # finally last line

try:
  # try first dedent line
  f()
# try last dedent line
except:
  # except dedent first line
  discard
# except dedent last line
finally:
  # finally first dedent line
  discard
# finally last dedent line

for i in 0 .. 1: # for colon line
  # for first line
  discard

case a # case line
of true: # of colon line
  # of first line
  discard
else: # case else colon line
  # case else first line
  discard

f do() -> int: # do colon line
  # do first line
  discard
  discard

block: # block colon line
  # block first line
  discard
  discard

let x = proc(): int = # lambda eq line
  # lambda first line
  discard
  discard
while false: # while colon line
  # while first line
  discard

static: # static colon line
  # static first line
  discard

discard Object( # object eol
  # object first line
  field: 0, # field line
  field2:
    # field colon line
    # Field colon next line
    42,
)

a = b
## Doc comment after assignment
## needs to be double

block:
  result.add()
## Doc comment after indented statement
## needs to be double

abc and
  # dedented comment in infix
  def

abc and
  # indented comment in infix
  def

if abc and
    # dedented comment in infix
    def:
  discard

if abc and
    # indented comment in infix
    def:
  discard

a(
  b = c # comment after keyword parameter
)

a(
  b = c # dedented comment after keyword parameter
)

{.
  pragma # comment here
.}

let v = 52 # let all on one line
let v = addr output # let all on one line with command

let
  # let first line indented
  v = 53 # after v

var
  # var first line indented
  v = 53 # after v

let # let eol
  v # let ident after symbol
  :
    # let ident after colon
    int =
    # let ident after type
    # let ident after equals
    42 # let ident after value

const # const eol
  v # const ident after symbol
  :
    # const ident after colon
    int =
    # const ident after type
    # const ident after equals
    42 # const ident after value

let
  a = 4
  b = 5
  # let section postfix

const
  a = 4
  b = 5
  # const section postfix

discard
  # discard eol
  # discard first line
  54 # discard value

proc x() =
  discard # proc, impl and comment on one line

proc xxx() = # after proc before indented name
  discard

proc xxxx() = # proc eq line
  # proc first line
  discard

proc x() =
  ## A proc doc comment
  if true:
    numberOfCharsRead -= 2 # handle Ctrl+Z as EOF

    for i in 0 ..< numberOfCharsRead:
      discard

proc x() =
  ## indented doc comment for proc
  ## that is long
  discard

proc x() =
  ## indented doc comment for proc
  ## with a dotexpr and a command
  echo a.b

proc f(): bool =
  ## Comment here
  ## another
  (true or false)

proc f(): bool =
  ## Comment here
  ## another
  if true:
    false
  else:
    ## comment
    ## comment 2
    if true:
      ## comment
      ## comment 2
      (true or false)
    else:
      false

proc f() =
  ## Doc comment only

proc f() =
  ## Doc comment only
  ## even two lines

proc a(
  v #[block]#
  : int,
  abc: int,
)

proc a(): int =
  ## Doc comment that needs body reordering
  ## even two lines again
  42

proc a(
    param: int, ## doc comment here
) =
  discard

command "a", "b", "c" # command eol comment

command "first arg", # first arg comment
  "second arg", # second arg comment
  "third arg" # third arg comment

command "first arg"
# comment after command

command 234 # command after ind

when false:
  command "first arg"
  # comment after command nest

when false:
  command "first arg" # comment command postfix nest

when false:
  command a.b
  # comment after command dotexpr nest

call() # call eol

echo dotexpr.dot # after dotexpr in command

# between two dotepxrs

dotexpr
# between dotexpr and dotonnewline
.dotonnewline

if true:
  echo dotexpr.dot # after dotexpr in command ind
  # between two dotepxrs ind

  dotexpr
  # between dotexpr and dotonnewline ind
  .dotonnewline

block:
  f.x
  # comment between the dots
  .z()
  # also after call
  .d() # far eol of dotexpr

  # after dotexpr ind

block:
  # no whitespace between the next two multilines
  functionCall(param)
  functionCall(param)

block:
  if true:
    discard
  # comment after if in block

discard
  # infix pre par
  (
    # infix parle
    # infix prefix
    a and # infix post-operator
    b # infix post
  ) # infix post par

block:
  block:
    discard
  # dedented comment post discard

block:
  if 2 >= 1 and 2 >= 1 and 2 >= 1: # some conditions:
    discard
  elif 2 >= 1 and 2 >= 1 and 2 >= 1: # some elif conds
    discard

  if 2 >= 1 and 2 >= 1 and 2 >= 1:
    # some conditions with a very long comment that wont fit on a line abacacsdcasdcasdsdcsdcsdc
    discard

  if 2 >= 1 and
      # some conditions
      2 >= 1 and 2 >= 1:
    discard

  while 2 >= 1 and 2 >= 1 and 2 >= 1: # some conditions:
    discard

  case
  # comment
  true
  of true: # comment
    discard
  else: #comment
    discard

checkUntilTimeout: # wait for nodes subscribe to finish
  discard

checkUntilTimeout( # wait for nodes subscribe to finish
  arg
)

checkUntilTimeout: # wait for nodes subscribe to finish
  discard

command a: # comment
  discard

{.gcsafe.}: # comment
  discard
