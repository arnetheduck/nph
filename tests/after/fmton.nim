proc getsFormatted(a, b: int) =
  discard

#!fmt: off
let
  myHandFormattedList
        :
   array[3, int]
 =
    [1, 2, 3]

#!fmt: on
proc hanging(indent: int, isUgly = true) =
  discard

block:
  #!fmt: off
  if   true   :
    discard
  #!fmt: on

  if false:
    discard

# Test block comments with trailing comments in fmt off
proc testBlockComments() =
  #!fmt: off
  #[ block comment ]# # trailing comment should stay
  var   ugly   =   1
  #[
    multiline
    block
  ]# # also should stay on same line
  #!fmt: on
  discard
