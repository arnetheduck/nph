var c3 = row[p] + (if runeA != runeB:
    1
  else:
    0)

var c3 = row[p] + (if runeA != runeB: 1 else: 0)

for a in 0..<1:
  discard

for a in 0..1:
  discard

# needs spaces
for a in ^1 .. ^2:
  discard

template ttt*: untyped =
  (block:
    xxx)[]


# command syntax with colon
discard xxxx "arg":
  yyy

# TODO
# (try: (discard rOk.tryError(); false) except ResultError[int]: true)

res.add (
  fff() do:
    yy() )

# we don't what `do` in let but need it in the above commend - this needs
# more investigation - this is important for templates calls like Result.valueOr
# which become ugly otherwise
let xxx = implicitdo:
    return xxx

let shortInfix = a*b*c+d*e*f+(a+b)*g

let longInfix = (
    a30*a21*a12*a03 - a20*a31*a12*a03 - a30*a11*a22*a03 + a10*a31*a22*a03 +
    a20*a11*a32*a03 - a10*a21*a32*a03 - a30*a21*a02*a13 + a20*a31*a02*a13 +
    a30*a01*a22*a13 - a00*a31*a22*a13 - a20*a01*a32*a13 + a00*a21*a32*a13 +
    a30*a11*a02*a23 - a10*a31*a02*a23 - a30*a01*a12*a23 + a00*a31*a12*a23 +
    a10*a01*a32*a23 - a00*a11*a32*a23 - a20*a11*a02*a33 + a10*a21*a02*a33 +
    a20*a01*a12*a33 - a00*a21*a12*a33 - a10*a01*a22*a33 + a00*a11*a22*a33
  )

if aaaaaaaaaaaaaaaaaaaa and bbbbbbbbbbbbbbbbbbbbbbb and ccccccccccccccccccccccccc and ddddddddddddddddd and fffffffffffffffff:
  discard

if (aaaaaaaaaaaaaaaa and bbbbbbbbbbbbbbbbbbbbbbbbbbbb) or (ccccccccccccccccccccccccccc and ddddddddddddddddddddd):
  discard
elif aaaaaaaaa and (bbbbbbbbbb or cccccccccccc and (dddddddddddddddd or eeeeeeeeeeeeeee or fffffffffffffff or gggggggggggggg)):
  discard


case aaaaaaaaaa
of aaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccc, ddddddddddddddddddddddd: discard
of aaaa: discard
of aaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccc, dddddddddddddddddddddd, aaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccc, dddddddddddddddddddddd:
  discard
else:
  discard

