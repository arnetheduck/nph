proc      getsFormatted(a, b : int    ) = discard

#!fmt: off
let
  myHandFormattedList
        :
   array[3, int]
 =
    [1, 2, 3]

#!fmt: on
proc hanging(indent: int,
             isUgly = true) = discard


block:
  #!fmt: off
  if   true   :
    discard
  #!fmt: on

  if   false   :
    discard