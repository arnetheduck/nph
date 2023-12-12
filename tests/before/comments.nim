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

x 324
  # A comment after

template x =
  ## A template doc comment
  try:
    discard
  except Exception:
    mixin `$` # A comment after mixin
    echo 4

type
  SingleValueSetting* {.pure.} = enum ## \
                      ## settings resulting in a single string value
    arguments,        ## experimental: the arguments passed after '-r'
    backend           ## the backend (eg: c|cpp|objc|js); both `nim doc --backend:js`
                      ## and `nim js` would imply backend=js
    gc {.deprecated.} ## gc selected
    mm                ## memory management selected

  FileSeekPos* = enum
    fspEnd ## Seek relative to end
    # text file handling:
  ## Position relative to which seek should happen.
  # The values are ordered so that they match with stdio
  # SEEK_SET, SEEK_CUR and SEEK_END respectively.

  Object = object # comment
    ## more comment
    field: int # Field comment
    # comment between fields
    field2: int ## Field comment again
  # and here

  Inherited = object of RootObj # inherited eol comment
                                # inherited next line indent comments
    f: int

  CaseObject = object # caseobj eol
    case k: bool # casetype eol
    of true, false: # of eol
      v: string # case field eol

  SomeAlias* = int ## alias eol
    ## alias next

  SomeAlias2 {.nodecl.} # after pragma
    = int ## alias2 eol

  SomeAlias3 # alias after symbol
    [T] = # alias after equals
    int # alias after type

  SomeAlias4 = SomeAlias3[int] ## after alias4
    ## more after alias4

  ## Some comment before whenobj
  WhenObject = object # whenobject object line
    when false: # when object false line
      discard

  NoField0* = object of RootObj ## comment eol
                                ## comment nl

  NoField1* = object of RootObj ## comment nofield1 eol
                                ## comment nl


when defined(somecond): # when colon line
  # when first line
  discard
# comment
else: # else colon line
  # else first line
  discard

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

proc # after proc before indented name
  xxx = discard

proc xxxx = # proc eq line
  # proc first line
  discard

proc x =
  ## A proc doc comment
  if true:
    numberOfCharsRead -= 2 # handle Ctrl+Z as EOF

    for i in 0 ..< numberOfCharsRead:
      discard

proc x = discard
  ## indented doc comment for proc
  ## that is long

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


for i in 0..1: # for colon line
  # for first line
  discard

case a # case line
of true: # of colon line
  # of first line
  discard
else: # case else colon line
  # case else first line
  discard

f do -> int: # do colon line
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
  field2: # field colon line
    # Field colon next line
    42
)

a = b
## Doc comment after assignment
## needs to be double

proc ffff =
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
   def: discard

if abc and
   # indented comment in infix
   def: discard

a(
  b = c # comment after keyword parameter
)

a(
  b = c
# dedented comment after keyword parameter
)

{.pragma # comment here
 .}

proc a(v#[block]#: int, abc: int)

let
  # let first line indented
  v = 53 # after v

var
  # var first line indented
  v = 53 # after v

discard # discard eol
  # discard first line
  54 # discard value

block:
  f
  .x
  # comment between the dots
  .z()
  # also after call
  .d()

proc f: bool =
  ## Comment here
  ## another
  (true or false)

proc f: bool =
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
