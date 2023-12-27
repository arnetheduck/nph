# Introduction

`nph`` is an opinionated source code formatter for the Nim language, aiming to
take the drudgery of manual formatting out of your coding day.

Following the great tradition of [`black`](https://github.com/psf/black/),
[`prettier`](https://prettier.io/), [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html)
and other AST-based formatters, it discards existing styling to create a
consistent and beautiful codebase.

## Priorities

`nph` aims to format code in such way that:

* it remains semantically unchanged, aka correct (!)
  * the AST is checked for equivalence before writing the formatted code to
    disk - on mismatch, the code is left untouched
* it remains simple, consistent and pleasant to read
  * _most_ code should look as good as or better than its hand-formatted
    counterpart
* diffs are kept at a minimum
  * diff-inducing constructs such as vertical alignment are avoided, for more
    productive merges
  * formatting the same code again results in no differences
* it broadly follows the [Status Nim style guide](https://status-im.github.io/nim-style-guide/)
  and [NEP1](https://nim-lang.org/docs/nep1.html)
  * this is tool aimed at making collaboration easier, with others and your
    future self
  * where NEP1 contradicts itself or these priorities, these priorities have
    precedence

The formatting rules are loosely derived from other formatters that already have
gone through the journey of debating what "pleasing to read" might mean while
making adaptations for both features and quirks of the Nim parser.

If in doubt, formatting that works well for descriptive identifiers and avoids
putting too much information in a single like will be preferred.

If something breaks the above guidelines, it's _likely_ a bug.
