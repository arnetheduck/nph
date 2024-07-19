proc a0() =
  discard

proc a1(v: int) =
  discard

proc a2(
    aaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccccccc,
      vvvvvvvvvvvvvvvvvvvvvv: string
) =
  discard

proc a3(
    aaaaaaaaaaaaaaaaaaaaaaa: int,
    bbbbbbbbbbbbbbbbbbbbbb: int,
    ccccccccccccccccccccccccccc: int,
    vvvvvvvvvvvvvvvvvvvvvv: string,
) =
  discard

proc a4(
    aaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccc: string
): Ddddddddddddddddddddddddd =
  discard

proc a5(v: int) {.nimcall.} =
  discard

proc a6(
    v: int
) {.
    nimcall, pragma2, pragma3, praaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaagma,
    rrr
.} =
  discard

proc a7(
    T: typedesc[
      Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbbbbbbbbb |
        Cccccccccccccccccccccccccc | Dddddddddddd
    ],
    aaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc, ddddddddddddddddddddddd;
) =
  discard

proc aaaaaaaaa*[
    Aaaaaaaaaaaaaaaaaaaaaaaa, Bbbbbbbbbbbbbbbbbbbbbbbbbb, Cccccccccccccccccccccc,
      Dddddddddddddddddddddd
](
    aaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbb, cccccccccccccccc,
      ddddddddddddddddddd: int
): ReturnType[
    Nested[
      Aaaaaaaaaaaaaaaaaaaaaa, Bbbbbbbbbbbbbbbbbbbbbbbbbbb, Cccccccccccccccccccccccc,
      Dddddddddddddddddddddddd, Eeeeeeeeeeeeeeeeeeeee,
    ]
] =
  discard

proc aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(
    v: int
) =
  discard

proc aaa*(v: int)
proc aaa[A, B, C](v: int)
proc aaaa*[A, B, C](v: int)
proc aaaaa[A, B, C](
  aaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbb, cccccccccccccccc,
    ddddddddddddddddddd: int
)

proc aaaa*[
  Aaaaaaaaaaaaaaaaaaaaaaaa, Bbbbbbbbbbbbbbbbbbbbbbbbbb, Cccccccccccccccccccccc,
    Dddddddddddddddddddddd
](v: int)

proc aaaaaaaaa*[
  Aaaaaaaaaaaaaaaaaaaaaaaa, Bbbbbbbbbbbbbbbbbbbbbbbbbb, Cccccccccccccccccccccc,
    Dddddddddddddddddddddd
](
  aaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbb, cccccccccccccccc,
    ddddddddddddddddddd: int
)

proc aaaaaaaa[T: Aaaa](v: int)
proc aaaaaaaa[
  Tttttttttttttttttttttttttttttttttttttttttt: Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
](v: int)

proc aaaaaaaa[T: Aaaa, S: static int](v: int)
proc aaaaaaaa[
  T:
    Aaaaaaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbbbbbbbb | Cccccccccccccccccccccccccc
](v: int)

proc aaaaaaaaaaa(
  v:
    Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbbbbbbbbb |
    Cccccccccccccccccccccccccc
)

aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 42 + 33 +
  44

command aaaaaaaaaaaaaaaaaaaaaa,
  aaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaa,
  bbbbbbbbbbbbbbbb
functionCall(
  aaaaaaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaaa,
  aaaaaaaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaa,
)

aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(
  aaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaaaaa
)

aasdcsaa(
  aaaaaaaaaa = bbbbbbbbbbb,
  ccccccccccc = ddddddddddddd,
  eeeeeeeeeeee = ffffffffffff,
  gggggg,
  hhhhhhhh,
)

type Ap = proc()
type Bp = proc
type Cp = proc(v: int)
type Dp = proc() {.nimcall.}
type Ep = proc {.nimcall.}
type Fp = proc(
  aaaaaaaaaaaaaaaaa: int,
  bbbbbbbbbbbbbbb = proc() =
    discard,
  cccccccccccccccccc = 30,
)

type Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = proc {.bbbbbbbbbbbbbbbbbbbbbbbb.}
type Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa =
  proc(aaaaaaaa: Bbbbbbb, ccccccccc: Dddddddddddd, eeeeee: Ffffff)

type Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = proc(
  aaaaaaaa: Bbbbbbb,
  ccccccccc: Dddddddddddd,
  eeeeeeeeeeeee: Ffffffffffffffff,
  gggggggggggggg: Hhhhhhhhhhhhhhhhhhhh,
)

proc `.()`() =
  discard

proc `[]`() =
  discard

proc `[`() =
  discard

proc exact88charswithequals(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa: int) =
  discard

proc exact89charswithequals(
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa: int
) =
  discard

check not (compiles do:
  result:
    int8 = 6)
