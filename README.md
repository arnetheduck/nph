# nph

nph is an opinionated source code formatter for the Nim language, aiming to
take the drudgery of manual formatting out of your coding day.

Following the great tradition of [`black`](https://github.com/psf/black/),
[`prettier`](https://prettier.io/), [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html)
and other AST-based formatters, it discards existing styling to create a
consistent and beautiful codebase.

## Usage

Install `nph`, then run it on some files:

```sh
# Format the given files in-place
nph file0.nim file1.nim

# Format the given files, writing the formatted code to /tmp
nph file0.nim file1.nim --outdir:/tmp

# Format an entire directory
nph src/

# Use --check to verify that a file is formatted correctly - useful in CI
nph --check somefile.nim || echo "Not formatted!"

# You can format stuff as part of a pipe using `-` as input:
echo "echo 1" | nph -
```

## Installation

Download binaries from the [releases page](https://github.com/arnetheduck/nph/releases/tag/latest).

Versioned releases will be made availabe in the future.

`nph` can be also compiled or installed using `nimble` version `v0.14.2`+:

```sh
nimble -l setup
nimble build
```

For bonus points, replace `nimpretty` with a symlink to `nph` - similar
command line options are supported ;)

## Editor integration

* [VSCode](https://marketplace.visualstudio.com/items?itemName=arnetheduck.vscode-nph) (`ext install arnetheduck.vscode-nph`)

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
* it broadly follows the [Status Nim style guide](https://status-im.github.io/nim-style-guide/)
  and [NEP1](https://nim-lang.org/docs/nep1.html)
  * this is tool aimed at making collaboration easier, with others and your
    future self
  * where NEP1 contradicts itself, see above

The formatting rules are loosely derived from other formatters that already have
gone through the journey of debating what "pleasing to read" might mean while
making adaptations for both features and quirks of the Nim parser.

If in doubt, formatting that works well for descriptive identifiers and avoids
putting too much information in a single like will be preferred.

## FAQ

### Why use a formatter?

A formatter removes the tedium of manually adding structure to code to make it
more readable - overlong lines, inconsistent indentation, lack of visual
structure and other small distractions quickly nibble away at the mental budget
available for writing code while a formatter solves this many many other things
at the press of a button.

When you work with others, debates and nitpicking over style go away and
collaborative efforts can focus on substance instead.

Finally, the code is likely to look better - manually formatting code takes a
lot of effort which ultimately can be spent better elsewhere - as such, poorly
formatted code ends up being more common than not.

### But I've spent a significant part of my life realigning code and now it's lost!

https://en.wikipedia.org/wiki/Sunk_cost

### How do I introduce `nph` in an existing codebase?

Assuming `git` is used, format all code using `nph`, put it in a single commit
and add a CI rule to ensure that future commits are all formatted using the same
`nph` version.

Formatting commits can be ignored for the purpose of `git blame` by adding a
file named `.git-blame-ignore-revs` containing the formatted source code to the
root of the project:

```sh
cd myproject

# Format all source code with nph
find -name "*.nim" -exec nph {} \;

# Create a single commit with all changes
git add -A && git commit -m "Formatted code with nph"

# Record the commit hash in the blame file
echo "# Formatted code with nph" >> .git-blame-ignore-revs
echo $(git rev-parse HEAD) >> .git-blame-ignore-revs
```

then configure git to use it:
```sh
git --global config blame.ignoreRevsFile .git-blame-ignore-revs
```

The same strategy can be used when upgrading `nph` to a new version that
introduces formatting changes.

### `nph` complains about my code!

One of several things could have happened:

* The code was not valid enough - `nph` can only parse valid Nim grammar and
  while it would be nice to handle partially formatted stuff gracefully, we're
  not there yet.
* The parser has a bug and is unable to parse valid Nim code
  * Probably you can move some comments around to make it work!
* the formatter has a bug and the resulting formatting is invalid
  * Probably you can move some comments around to make it work!
* the AST equivalence checker complains
  * This often happens in complex expressions such as `do`  and parenthesis used
    for indent purposes where the Nim grammar has ambiguities and parsing
    complexity - it can usually be worked around by simplifying complex
    expressions, introducing a template or similar
  * It could also be that the AST checker is too strict - the Nim parser will
    generate different AST:s depending on whitespace even if semantically there
    is no difference

Regardless of what happened, `nph` takes the conservative approach and retains
the original formatting!

If you have time, try to find the offending code snippet and submit an issue.

### Why the cited formatters in particular?

* `black` because of our syntactic similarity with Python and its
  [stability policy](https://black.readthedocs.io/en/stable/the_black_code_style/index.html#stability-policy)
* `prettier` for its wisdom in how formatting [options](https://prettier.io/docs/en/option-philosophy)
  are approached and for the closeness to user experience of its developers
* `clang-format` for being the formatter that made me stop worrying about
  formatting
  * its secret sauce was treating formatting as a balancing of priorities rather
    than a mechanical stringification using a [lowest-penalty](https://youtu.be/s7JmdCfI__c?t=640)
    algorithm

### What is meant by consistency?

* Similar constructs are formatted with similar rules
  * Does it look like a list? Format it with list-like rules regardless if
    its a parameter list, array of values or import list
* Original styling is generally not preserved - instead, the formatting is based
  on the semantic structure of the program
* Spacing emphasizes structure and control flow to help you read the code

`nph` makes your code consistent without introducing hobgoblins in your mind!

### Why are there no options?

The aim of `nph` is to create a single consistent style that allows you to
focus on programming while `nph` takes care of the formatting, even across
different codebases and authors.

Consistency helps reading speed by removing unique and elaborate formatting
distractions, allowing you, the experienced programmer, to derive structural
information about the codebase at a glance.

The style might feel unfamiliar in the beginning - this is fine and not a reason
to panic - a few weeks from now, you'll forget you ever used another one.

### Do you accept style suggestions and changes?

Yes! The project is still in its early phase meaning that the style is not yet
set in stone.

To submit a proposal, include some existing code, how you'd like it to be
formatted and an option-free algorithm detailing how to achieve it and how the
outcome relates to the above styling priorities.

When in doubt, look at what other opinionated formatters have done and link to
it!

Eventually, the plan is to adopt a [stability policy](https://black.readthedocs.io/en/stable/the_black_code_style/index.html#stability-policy)
similar to `black`, meaning that style changes will still be accepted, but
introduced only rarely so that you don't have to worry about massive PR-breaking
formatting diffs all the time.

### Why does the formatting code look an awful lot like the Nim compiler renderer?

Because it is based on it, of course! As a starting point this is fine but the
code would benefit greatly from being rewritten with a dedicated formatting
AST.

In particular, the comment handling is done in a hand-wavy manner with bits and
pieces of the old code mixed with new heuristics resulting in quite the mess.

### Should it be upstreamed?

Maybe parts - feel free to make PR:s to the Nim repo from this codebase! That
said, the aim of a compiler is to compile while a formatter formats - we are not
the same.

### What about `nimpretty`?

`nimpretty` formats tokens, not the AST. Use whichever you like better, but keep
a backup if you don't use `nph` :)

### Why 88 characters?

This is an experiment.

Astute and experienced programmers have noticed two things: longer variable
names aren't that bad and monitors have gotten bigger since the 80 standard was
set.

Going beyond allows code that uses descriptive names to look better - how much
extra is needed here is an open question but 10% seems like a good start for a
language like Nim which defaults to 2-space significant indent and a naive
module system that encourages globally unique identifiers with longer names.

### What about comments?

`nph` currently touches comments as little as possible - specifically, they
are not re-flowed or re-aligned and the aim is to make them sticky to where they
were originally written.

Improvements in this area are much welcome - the compiler AST was not really
written with comment preservation in mind and `nph`'s handling is not great.

### How are blank lines handled?

Coming up with a fully automatic rendering of blank lines is tricky because they
are often used to signal logical groupings of code for which no other mechanism
exists to represent them.

`nph` current will:

* generally retain blank space in code but normalise it to a single line
* insert blanks around complex statements

This strategy is expected to evolve over time, including the meaning of
"complex".

### What features will likely not be added?

* formatting options - things that change the way the formatting is done for
  aesthetic reasons - exceptions here might include options that increase
  compatiblity (for example with older Nim versions)
* semantic refactoring - reordings of imports etc - the focus is on style only

### What's with the semicolons?

Nim's grammar unfortunately allows the use of either `,` or `;` in some places
with a subtly different AST being produced which _sometimes_ has a semantic
impact - `nph` takes a conservative approach here and uses `;` to avoid
ambiguity.

Future versions may try to identify where `,` can be used safely - very few
programmers _really_ want `;` judging from the frequency with which it is used.

Regardless, you can usually type either and `nph` will clean it up.
