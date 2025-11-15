# Changelog

# Unreleased

New features:

* `--diff` flag to show unified diff of formatting changes without modifying
  files
* `--color` / `--no-color` flags for colored diff output (requires `--diff`)
* `.nph.toml` config file support for project-level configuration
* `--exclude` flag to replace default exclusion patterns
* `--extend-exclude` flag to add patterns to default exclusions
* `--include` flag to customize which files to format
* `--config` flag to specify a custom config file
* Default exclusion patterns for common directories (build, dist, .git,
  nimcache, etc.)
* Files passed explicitly on CLI bypass all exclude filters (matching Black's
  behavior)
* CLI options override config file settings
* pre-commit integration support with `nph` hook

# 0.6.1

No formatting changes!

* nimble compatibility updates (fixes `nimble install nph` sometimes)
* osx arm64 binaries

# 0.6.0

* avoid putting `,` on its own line after complex expressions, where possible
* allow compiling with a wider range of Nim 2.0 versions, hoping it won't break
* a few bugfixes

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

