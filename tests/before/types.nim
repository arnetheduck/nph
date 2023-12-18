type A = int

type B = A | B
type Bbbbbbbbbbbbbbbbbbbbbbbbbbbbb = Aaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbb | Cccccccccccccccccccccc | Dddddddddddddddddddddd

type A[T: Aaaaaaaaaaaaaaaaaaa|Bbbbbbbbbbbbbbbbbbbbbbbb|Cccccccccccccccccccc|Dddddddddddddddd|Eeeeeeeeeeeeeeeeeee] = Bbbbbbbbbbbbbbbbbb[T]

proc f(a: Aaaaaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbb | Ccccccccccccccccccccccccccc | Ddddddddddddddddddddddddd | Eeeeeeeeeeeeeeeee)


type FieldObject = object
  aaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbb, cccccccccccccccccccc, dddddddddddddddddddd, eeeeeeeeeeeeeeeeeeee, fffffffffffffffffff, ggggggggggggggggggggggg: int
  aaaaaaaaaaaaaaa*, bbbbbbbbbbbbbbbbb*, cccccccccccccccccccc*, dddddddddddddddddddd*, eeeeeeeeeeeeeeeeeeee*, fffffffffffffffffff*, ggggggggggggggggggggggg*: int

type CaseObject = object
  case f: bool
  of false:
    vfalse: int
  of true:
    vtrue: int

type CaseObject = object
  case f {.aaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccc, ddddddddddddddddddddd, eeeeeeeeeeeeeeeeeee.}: bool
  of false:
    vfalse: int
  of true:
    vtrue: int

type PragmaObject {.objectPragma, aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc, dddddddddddddddddddd.} = object
  vfalse {.fieldPragma.}: int
  vpublic* {.fieldPragma.}: int

  vfalse {.fieldPragma, aaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccccccccc, dddddddddddddddddddddd.}: int
  vpublic* {.fieldPragma.}: int

