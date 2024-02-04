# The `nph` style

As outlined in the [introduction](./introduction.md#priorities), `nph` strives
to maintain correctness and consistency across various language constructs with
a preference for styles that work well for collaboration.

This section of the book documents some of the style choices and why they were
made.

<!-- toc -->

## Overview

`nph` generally approaches formatting by considering several choices of
formatting and choosing a reasonable one based on a number of heuristics.

To get an idea what the format looks like, here's a typical proc definition -
everything fits on one line, nice!

```nim
proc covers(entry: AttestationEntry; bits: CommitteeValidatorsBits): bool =
  ...
```

If we add more arguments, it starts getting long - nph will try with a version
where the arguments sit on a line of their own:

```nim
proc addAttestation(
    entry: var AttestationEntry; attestation: Attestation; signature: CookedSig
): bool =
  ...
```

The above idea extends to most formatting: if something is simple, format it in
a simple way - if not, use a bit of style to break down what's going on into
more easily consumable pieces - here's a function with several information-dense
parameters and a complex return type:

```nim
proc validateBlsToExecutionChange*(
    pool: ValidatorChangePool;
    batchCrypto: ref BatchCrypto;
    signed_address_change: SignedBLSToExecutionChange;
    wallEpoch: Epoch
): Future[Result[void, ValidationError]] {.async.} =
  ...
```

```admonish info "Example styling"
The examples are illustrative and not based on exact rendering semantics - in
particular, a different line length was used to keep them readable
```

## Lists

Lists appear frequently in source code: import modules, parameter lists, arrays
and sequence initializers, function call parameters, etc etc.

Generally, list rendering is done according to a number of heuristics, striving
to balance information density with the use of available screen space.

If the whole list fits on the current line, it is rendered in-place. Short
sequences, single-parameter functions etc usually fit into this category:

```nim
import folder/module

const v = [1, 2, 3]

type T = proc(a, b: int)
```

If it doesnt fit from the current position, we try fitting it in one line on a
new line - this frequently happens with parameter lists and constants where the
name takes up space

```nim
import
  folder/[module1, module2, module3]

const
  mylongvariablename = [
    100000000, 2000000000, 300000000000
  ]

proc function(
  param0: int, param1: int, param2: int
)
```

If the list still doesn't fit on a single line, we look at the contents to
choose between two styles.

If it contains complex complex values, we render one value per row - this
happens most often for function parameters and other information-dense constructs.

```nim
import
  folder/[module1, module2, module3],
  folder2/[
    module4, module5, module6, module7, module8,
    module9
  ]

let
  myVariable = [
    functionCall(a, b, c),
    functionCall(a, b, c, d),
  ]

functionCall(
  functionCall(a, b, c),
  functionCall(a, b, c, d),
)
```

```admonish info "Extra separator"
In the long style, we'll insert an extra separator at the end where permissible -
this makes it easier to reorder entries and reduces git conflicts!
```

For simple values, we use a compact style that fits several items per row:

```nim
const
  values = [
    10000000, 2000000000, 3000000000,
    40000000, 5000000000
  ]

functionCall(
  10000000, 2000000000, 3000000000,
  40000000, 5000000000
)
```

```admonish info "What is simple?"
* literals (`2`, `"string"` etc)
* simple identifiers (`myvar` etc)
* dot expressions of the the above (`myObject.field`)
```

### Parameter lists

Parameter lists, such as function parameters and generics, are rendered using
the above list style. In the AST, each parameter group is made up of 3
components: one or more names, a type and a default.

If both type and default are missing, we disambiguate parsing multiple names and
groups using a `;`.

```nim
# Usually we can use comma to separate items
proc f(a, b: int, c: float)

# A semicolon is necessary to ensure `T` is interpreted as a type and not part
# of the `v: static int` identifier group
proc g[T; v: static int]

# Semicolons are also significant for type-less parameters - the following two
# templates parse to different ASTs:
template weare(a; b) = discard
template notthesame(a, b) = discard

# Semicolons cannot be used at all for inline procedures:
proc f(
  myParameter = 0,
  callback: SomeCallback =
    proc() =
      discard
  ,
  nextParameter = 1,
)
```

### Infix operators

`nph` puts spaces around infix operators such as `and` and `..`.

Although NEP1 suggests not having spaces around `..` and `..<` in particular,
this creates an exception to the normal infix spacing rules.

In spite of this recommendation, lots of code out there maintains spaces around
the operators which makes decision based on "existing practice" hard.

Adding to the complexity is in order to not break the AST, one would have to
take care to remove the spaces only in cases where the infix is not followed by
another operator (such as `-`) - this means that we _sometimes_ have to put
spaces around these infixes and sometimes not, leading to irregularity.

Since there's no consensus in existing code at the time of writing, the rule is
irregular and causes implementation complexity, `nph` formats `..` and `..<`
with spaces.

## Expressions

Expressions appear in many places, such as after certain keywords (`return`,
`yield`), as part of control flows (`if`, `while`), in assignments etc.

Whenever possible, `nph` will try to keep the full expression on a single line:

```nim
let myvariable = somelongexpression(abc)
```

If this is not possible, the second preference is to move the whole expression
to a new line, assuming it fits:

```nim
let myvariable =
   someevenlongerexpression(abc, def)
```

If the expression still doesn't fit, we'll split it up on multiple lines:

```nim
let myvariable = someevenlongerexpression(
  aaa, bbb, ccc, ddd
)
```

Certain expressions linked by related keywords that don't fit on a single line
will also be moved to a new line - for example, a multi-line `if`/`else` nested
in a `return` will be lined up like so:

```nim
return
  if condition:
    complex(call)
  else:
    alsocomplex(call)
```
