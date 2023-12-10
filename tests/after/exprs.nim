var c3 =
  row[p] + (if runeA != runeB:
      1
    else:
      0
    )
var c3 =
  row[p] + (if runeA != runeB:
      1
    else: 0
    )

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
      ## comment
      (true or false)
    else:
      false

for a in 0 ..< 1:
  discard
for a in 0 .. 1:
  discard
# needs spaces
for a in ^1 .. ^2:
  discard

template ttt*(): untyped =
  (block:
    xxx
  )[]

# command syntax with colon
discard xxxx "arg":
    yyy

res.add fff do:
    yy()

# we don't what `do` in let but need it in the above commend - this needs
# more investigation - this is important for templates calls like Result.valueOr
# which become ugly otherwise

let xxx =
  implicitdo:
    return xxx
