# Changelog

# 0.5.1

* fix a bug in the length computation of postfix-commented items
* fix a bug in statement list expression rendering in calls
* fix a bug in whitespace retention between calls

# 0.5

* allow function calls to partially fill line in assignments
* trailing commas also in simple multi-line lists
* polish

# 0.4.1

* fix comment-eating bug in infix

# 0.4

* `..` and `..<` gained spaces around them like all other infixes
* chains of dot-calls get aligned
* prefer putting things on a new line if this helps fit the whole expression
* add `.` to simple expressions
* fix several cases of over-indent
* render infix expressions with operator ending the line where possible

# 0.3

* Back to `,` as parameter separator - this helps compatibility with inline procs
* 100% Nim compiler/stdlib compatibility
* New manual at https://arnetheduck.github.io/nph/
* Assorted bugfixes

# 0.2

* Initial versioned release
* Comes with VSCode integration and 99% compatibibility

