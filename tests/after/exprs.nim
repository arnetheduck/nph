var c3 = row[p] + (if runeA != runeB: 1 else: 0)

var c3 = row[p] + (if runeA != runeB: 1 else: 0)

while (node = node.next; node != nil):
  discard

while (var a = 0; inc a; a += 42; a > 0):
  discard

for a in 0 ..< 1:
  discard

for a in 0 .. 1:
  discard

# needs spaces
for a in ^1 .. ^2:
  discard

template ttt*(): untyped =
  (
    block:
      xxx
  )[]

# single-line expressions with varying amounts of pars

let
  x0 = (discard; false)
  x1 = if (discard; false): 0 else: 1
  x2 = (if (discard; false): 0 else: 1)
  x3 = (if (discard; false): (0) else: (1))

# command syntax with colon
discard xxxx "arg":
  yyy

(
  try:
    (discard rOk.tryError(); false)
  except ResultError[int]:
    true
)

let shortInfix = a * b * c + d * e * f + (a + b) * g

let mediumInfix =
  aaaaaaaaaaaaa + bbbbbbbbbbbbbbb + ccccccccccccccc + dddddddddddddddd +
  eeeeeeeeeeeeeeeeeeeee + fffffffffffffff * ggggggggggggggg + hhhhhhhhhhhhhhh +
  iiiiiiiiiiiiii + jjjjjjjjjjjjjjjjj + kkkkkkkkkk
let parenInfix =
  (
    aaaaaaaaaaaaa + bbbbbbbbbbbbbbb + ccccccccccccccc + dddddddddddddddd +
    eeeeeeeeeeeeeeeeeeeee + fffffffffffffff
  ) * (
    ggggggggggggggg + hhhhhhhhhhhhhhh + iiiiiiiiiiiiii + jjjjjjjjjjjjjjjjj +
    kkkkkkkkkkkkk
  )
let parenShortInfix =
  (aaaaaaaaaaaaa + eeeeeeeeeeeeeeeeeeeee) * (
    ggggggggggggggg + hhhhhhhhhhhhhhh + iiiiiiiiiiiiii + jjjjjjjjjjjjjjjjj +
    kkkkkkkkkkkkkkkk
  )

let longInfix = (
  a30 * a21 * a12 * a03 - a20 * a31 * a12 * a03 - a30 * a11 * a22 * a03 +
  a10 * a31 * a22 * a03 + a20 * a11 * a32 * a03 - a10 * a21 * a32 * a03 -
  a30 * a21 * a02 * a13 + a20 * a31 * a02 * a13 + a30 * a01 * a22 * a13 -
  a00 * a31 * a22 * a13 - a20 * a01 * a32 * a13 + a00 * a21 * a32 * a13 +
  a30 * a11 * a02 * a23 - a10 * a31 * a02 * a23 - a30 * a01 * a12 * a23 +
  a00 * a31 * a12 * a23 + a10 * a01 * a32 * a23 - a00 * a11 * a32 * a23 -
  a20 * a11 * a02 * a33 + a10 * a21 * a02 * a33 + a20 * a01 * a12 * a33 -
  a00 * a21 * a12 * a33 - a10 * a01 * a22 * a33 + a00 * a11 * a22 * a33
)

let longArray = [
  aaaaaaaaaaaaa, bbbbbbbbbbbbbbb, ccccccccccccccc, dddddddddddddddd,
  eeeeeeeeeeeeeeeeeeeee, fffffffffffffff, ggggggggggggggg, hhhhhhhhhhhhhhh,
  iiiiiiiiiiiiii, jjjjjjjjjjjjjjjjj, kkkkkkkkkk,
]

if aaaaaaaaaaaaaaaaaaaa and bbbbbbbbbbbbbbbbbbbbbbb and ccccccccccccccccccccccccc and
    ddddddddddddddddd and fffffffffffffffff:
  discard

if (aaaaaaaaaaaaaaaa and bbbbbbbbbbbbbbbbbbbbbbbbbbbb) or
    (ccccccccccccccccccccccccccc and ddddddddddddddddddddd):
  discard
elif aaaaaaaaa and (
  bbbbbbbbbb or
  cccccccccccc and
  (dddddddddddddddd or eeeeeeeeeeeeeee or fffffffffffffff or gggggggggggggg)
):
  discard

let prefix = if ifstmtthatfitsonasinglelinewithlongcond: "[Valid]   " else: "[Invalid] "

let prefix2 =
  if someprettylongconditionhereexpressiondoestfit: "[Valid]   " else: "[Invalid] "
let prefix3 =
  if someprettylongconditionhereexpressiondoestfitatallabcabcabcabc:
    "[Valid]   "
  else:
    "[Invalid] "

# short if in short if
if true:
  if false:
    discard

let a =
  case x
  of 2: 4
  else: 4

case aaaaaaaaaa
of aaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccc,
    ddddddddddddddddddddddd:
  discard
of aaaa:
  discard
of aaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccc,
    dddddddddddddddddddddd, aaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
    ccccccccccccccccccccccc, dddddddddddddddddddddd:
  discard
of aaaaaaaaaaaaaaaaaaaaaaaaaa .. bbbbbbbbbbbbbbbbbbbbbbbb,
    dddddddddddddddddddddddddd .. ccccccccccccccccccccccc:
  discard
else:
  discard

discard a .. b
discard a ..< b
discard a ..^ b
discard a .. b
discard a ..^ b
discard a ..< b
discard a .. ^b
discard a .. <b
discard a .. -b
discard -1 .. -2

aaaaaaa.bbbbbbbbb
.longdotcall().ccccccccc.dddddddddd.eeeeeeeee
.sdcsd(a0000000, b000000, c000000).fffffff.ggggggg.hhhhhhhh.csdcsdcs.sdcsdcsd.csdcsdcsdcsd.csdcsdcs.dcsdcsdcsdcs.sdc

mynums = myNums
  .replace1("one", "o1ne")
  .replace2("two", "t2wo")
  .replace3("three", "t3hree")
  .replace5("four", "f4our")
  .replace5("five", "f5ive")
  .replace6("six", "s6ix")
  .replace7("seven", "s7even")

let xxxxxxxxx = block:
  f()
  v

let yyyyyyyyyy = aaaaaaaaaaaaaaaaaaaaaaaaa.ffffffffffffff(
  aaaaaaaaa, bbbbbbbbbbbbbbbbbb, cccccccccccccccccccc
)

discard
  aaaaaaaaa and (
    aaaaaaaaaaaaaaaaaaaaaaaa and (
      aaaaaaaaaaaaaaaaaaaaa and bbbbbbbbbbbbbbbbbbbbbbb and
      (cccccccccccccccccccccc and ddddddddddddddddddd)
    )
  )

# Dot expressions
let test2 = someSimpleResult.fff(v).valueOr:
  3

let test2 = someSimpleResult(
  aaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbb, ccccccccccccccccccccc, ddddddddddddd
).valueOr:
  3

var aaaaaaaaaaaaaaaaaaaaaaaa =
  bbbbbbbbbbbbbbbbbbbb.cccccccccccccccccccc[].ddddddddddddddddddddd

aaaaaaaaaaaaaaaaaaaaaaaa =
  bbbbbbbbbbbbbbbbbbbb.cccccccccccccccccccc[].ddddddddddddddddddddd

let aaaaaaaaaaaaaaaaa = [
  f(aaaaaaaaaaaa, bbbbbbbbbbbbbbbbbb, ccccccccccccccccccccc),
  ddddddddddddddd(eeeeeeeeeeeee, ffffffffffffff, ggggggggggggg),
]

let bbbbbbb = [
  "aaaaaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbbbbbbbbbbb",
  "ccccccccccccccccccccccccccccc", "dddddddddddddddddddddd",
]

let bbbbbbccccb = [
  int64 533, 444444444444444444, 555555555555555555, 6666666666666666, 6777777777777777,
  888888888888888,
]
