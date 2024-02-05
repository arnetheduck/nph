# Style examples - compile with `-d:nphBookExamples` to regenerate them
# for the book

proc function(par0: SomeType): bool =
  discard

proc function(par0: SomeType, par1: SomeType): bool =
  discard

proc function(
    par0: SomeType, par1, par2: SomeOtherType, par3 = default(SomeType)
): bool {.inline.} =
  discard

import dir/module

const v = [1, 2, 3]

type T = proc(a, b: int)

import dir/[module1, module2, module3, module4]

const mylongvariablename = [100000000, 200000000, 300000000]

proc function(param0: int, param1: int, param2: int)

import
  dir/[module1, module2, module3],
  dir2/[module4, module5, module6, module7, module8, module9]

let myVariable = [functionCall(a, b, c), functionCall(a, b, c, d)]

functionCall(functionCall(a, b, c), functionCall(a, b, c, d))

const values = [10000000, 2000000000, 3000000000, 40000000, 5000000000]

functionCall(10000000, 2000000000, 3000000000, 40000000, 5000000000)

proc f(
  myParameter = 0,
  callback: SomeCallback = proc() =
    discard
  ,
  nextParameter = 1,
)

let myvariable = shortexpression(abc)

let myvariable = someevenlongerexpression(abc, def)

let myvariable = someevenlongerexpression(aaa, bbb, ccc, ddd)

return
  if condition:
    complex(call)
  else:
    alsocomplex(call)
