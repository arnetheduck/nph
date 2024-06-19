# FAQ

## Why use a formatter?

A formatter removes the tedium of manually adding structure to code to make it
more readable - overlong lines, inconsistent indentation, lack of visual
structure and other small distractions quickly nibble away at the mental budget
available for writing code while a formatter solves this and many other things
at the press of a button.

When you work with others, debates and nitpicking over style go away and
collaborative efforts can focus on substance instead.

Finally, the code is likely to look better - manually formatting code takes a
lot of effort which ultimately can be spent better elsewhere - as such, poorly
formatted code ends up being more common than not.

## But I've spent a significant part of my life realigning code and now it's lost!

https://en.wikipedia.org/wiki/Sunk_cost

## How do I introduce `nph` in an existing codebase?

Assuming `git` is used, format all code using `nph`, put it in a single commit
and add a CI rule to ensure that future commits are all formatted using the same
`nph` version.

Formatting commits can be ignored for the purpose of `git blame` by adding a
file named `.git-blame-ignore-revs` containing the formatted source code to the
root of the project:

```sh
cd myproject

# Format all source code with nph
git ls-files | grep ".nim$" | xargs -n1 nph

# Create a single commit with all changes
git commit -am "Formatted with nph $(nph --version)"

# Record the commit hash in the blame file
echo "# Formatted with nph $(nph --version)" >> .git-blame-ignore-revs
echo $(git rev-parse HEAD) >> .git-blame-ignore-revs
```

then configure git to use it:
```sh
git config --global blame.ignoreRevsFile .git-blame-ignore-revs
```

The same strategy can be used when upgrading `nph` to a new version that
introduces formatting changes.

## `nph` complains about my code!

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

## Why the cited formatters in particular?

* `black` because of our syntactic similarity with Python and its
  [stability policy](https://black.readthedocs.io/en/stable/the_black_code_style/index.html#stability-policy)
* `prettier` for its wisdom in how formatting [options](https://prettier.io/docs/en/option-philosophy)
  are approached and for the closeness to user experience of its developers
* `clang-format` for being the formatter that made me stop worrying about
  formatting
  * its secret sauce was treating formatting as a balancing of priorities rather
    than a mechanical stringification using a [lowest-penalty](https://youtu.be/s7JmdCfI__c?t=640)
    algorithm

## What is meant by consistency?

* Similar constructs are formatted with similar rules
  * Does it look like a list? Format it with list-like rules regardless if
    its a parameter list, array of values or import list
* Original styling is generally not preserved - instead, the formatting is based
  on the semantic structure of the program
* Spacing emphasizes structure and control flow to help you read the code

`nph` makes your code consistent without introducing hobgoblins in your mind!

## Why are there no options?

The aim of `nph` is to create a single consistent style that allows you to
focus on programming while `nph` takes care of the formatting, even across
different codebases and authors.

Consistency helps reading speed by removing unique and elaborate formatting
distractions, allowing you, the experienced programmer, to derive structural
information about the codebase at a glance.

The style might feel unfamiliar in the beginning - this is fine and not a reason
to panic - a few weeks from now, you'll forget you ever used another one.

## Do you accept style suggestions and changes?

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

## Why does the formatting code look an awful lot like the Nim compiler renderer?

Because it is based on it, of course! As a starting point this is fine but the
code would benefit greatly from being rewritten with a dedicated formatting
AST - and here we are.

## Should it be upstreamed?

Maybe parts - feel free to make PR:s to the Nim repo from this codebase! That
said, the aim of a compiler is to compile while a formatter formats - we are not
the same.

## What about `nimpretty`?

`nimpretty` formats tokens, not the AST. Use whichever you like better, but keep
a backup if you don't use `nph` :)

## Why 88 characters?

This is an experiment.

Astute and experienced programmers have noticed two things: longer variable
names aren't that bad and monitors have gotten bigger since the 80 standard was
set.

Going beyond allows code that uses descriptive names to look better - how much
extra is needed here is an open question but 10% seems like a good start for a
language like Nim which defaults to 2-space significant indent and a naive
module system that encourages globally unique identifiers with longer names.

Automated formatting keeps most code well below this limit but the extra 10%
allows gives it some lenience - think of it as those cases where a prorgammer
would use their judgement and common sense to override a style guide
recommendation.

## What about comments?

Comments may appear in many different places that are not represented in the
Nim AST. When `nph` reformats code, it may have to move comments around in order
to maintain line lengths and introduce or remove indentation.

`nph` uses heuristics to place comments into one of several categories which
broadly play by similar rules that code does - in particular, indentation is
used to determine "ownership" over the comment.

The implementation currently tracks several comment categories:

* comment statement nodes - comments that appear with regular indent in
  statement list contexts (such as the body of a `proc`) as represented as such,
  ie as statement nodes and get treated similar to how regular code would
* node attachments - comments that are anchored to an AST node depending on
  their location in the code relative to that node:
  * prefix - anything leading up to a particular AST node - for example less
    indented or otherwise appearing before the node
  * mid - at midpoints in composite nodes - between the `:` and the body of an
    `if` for example
  * postfix - appearing after the node, meaning on the same line or more
    indented than the node

When rendering the code, `nph` will use these categories to guide where the
comment text should go, maintaining comment output in such a way that parsing
the file again results in equivalent comment placement.

## How are blank lines handled?

Coming up with a fully automatic rendering of blank lines is tricky because they
are often used to signal logical groupings of code for which no other mechanism
exists to represent them.

`nph` current will:

* generally retain blank space in code but normalise it to a single line
* insert blanks around complex statements

This strategy is expected to evolve over time, including the meaning of
"complex".

## What features will likely not be added?

* formatting options - things that change the way the formatting is done for
  aesthetic reasons - exceptions here might include options that increase
  compatiblity (for example with older Nim versions)
* semantic refactoring - the focus is on style only
  * `import` reording in particular changes order in which code executes!

## What's with the semicolons?

Nim's grammar unfortunately allows the use of either `,` or `;` in some places
with a subtly different AST being produced which _sometimes_ has a semantic
impact.

Parameters in particular are parsed using identifier groups where each group
consists of one or more names followed by an option type and default.

Names are separated by `,` - if the type and default are missing, a `;` is
needed to start a new group or the name would be added to the previous group
if a `;` was used originally to create a new group.

However, if the group has a default, `;` cannot be parsed because it's swallowed
in certain cases (`proc` implementations in particular) by the default value
parsing.

As such, `nph` will normalise usage of `,` and `;` to:

* Use `,` after a group that has a type and/or default
* Use `;` otherwise

Regardless, you can usually type either and `nph` will clean it up in such a way
that the AST remains unambiguous, compatible with all possible values and in
line with the common expectation that `,` is used where possible.
