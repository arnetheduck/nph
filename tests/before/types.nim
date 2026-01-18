type A = int

type TypeClass = A | B
type Bbbbbbbbbbbbbbbbbbbbbbbbbbbbb = Aaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbb | Cccccccccccccccccccccc | Dddddddddddddddddddddd | Eeeeeeeeeeeeeeeeeeeeeeeeee

type GenericAlias[T: Aaaaaaaaaaaaaaaaaaa|Bbbbbbbbbbbbbbbbbbbbbbbb|Cccccccccccccccccccc|Dddddddddddddddd|Eeeeeeeeeeeeeeeeeee] = Bbbbbbbbbbbbbbbbbb[T]

type GenericRef = GenType[A, ref B]
proc procTypeclass(a: Aaaaaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbb | Ccccccccccccccccccccccccccc | Ddddddddddddddddddddddddd | Eeeeeeeeeeeeeeeee)

type EmptyObject = object

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

type RefObject = ref object of RootObj
  reffield: int

type RefAlone = ref object of RootObj

type SimpleEnum = enum
  a, b, c

type LongStringEnum = enum
  aaa = "test"
  bbbbbbbbbbbbbbbbbbbbb = "cccccccccccccccccccc dddddddddddddddddddddddd  eeeeeeeeeeeeeeee fffffffffffff"
  bbbbbbbbbbbbbbbbbbbbbc = "2cccccccccccccccccccc dddddddddddddddddddddddd  eeeeeeeeeeeeeeee fffffffffffff"
  bbb = "xxxxx"

type NumberEnum = enum
  aaa = 332
  bbbbbbbbbbbbbbbbbbbbbcddddddddddddddddssssssssssssssssssscccccccccccccrrrrrrrrrrrrrrrrr = 424

type
  User = concept u
  SuperUser = concept u of User
