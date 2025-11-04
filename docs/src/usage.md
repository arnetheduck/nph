# Usage

```sh
# Format the given files in-place
nph file0.nim file1.nim

# Format the given files, writing the formatted code to /tmp
nph file0.nim file1.nim --outdir:/tmp

# Format an entire directory
nph src/

# Use --check to verify that a file is formatted correctly as `nph` would - useful in CI
nph --check somefile.nim || echo "Not formatted!"

# Show a diff of what would change without modifying files
nph --diff somefile.nim

# Show a colored diff (requires --diff)
nph --diff --color somefile.nim

# You can format stuff as part of a pipe using `-` as input:
echo "echo 1" | nph -
```

## Configuration file

You can configure `nph` using a `.nph.toml` file in your project root. CLI
options override config file settings.

```toml
# Completely replace default exclusions with your own patterns
exclude = [
  "build",
  "dist",
]

# Add to default exclusions (recommended - doesn't lose the defaults)
extend-exclude = [
  "tests/fixtures",
  "vendor",
]

# Customize which files to include (default: \.nim(s|ble)?$)
include = [
  "\.nim$",
  "\.nims$",
]
```

### Default exclusions

By default, `nph` excludes common directories that typically don't contain
source code:

- `.git`, `.hg`, `.svn` - version control
- `.nimble`, `nimcache` - Nim build artifacts
- `.vscode`, `.idea` - editor directories
- `__pycache__`, `.mypy_cache`, `.pytest_cache` - Python artifacts
- `.nox`, `.tox`, `.venv`, `venv`, `.eggs` - Python environments
- `node_modules` - JavaScript dependencies
- `_build`, `buck-out`, `build`, `dist` - common build directories

Use `extend-exclude` to add to these defaults, or `exclude` to replace them
entirely.

## CLI options

### Filtering options

#### `--exclude:pattern`

Completely replaces the default exclusion patterns with your own regex pattern.
Can be specified multiple times.

```sh
# Only exclude the build directory
nph --exclude:build src/
```

#### `--extend-exclude:pattern`

Adds a regex pattern to the default exclusions. Can be specified multiple times.
This is typically more useful than `--exclude` since you keep the sensible
defaults.

```sh
# Exclude vendor directory in addition to defaults
nph --extend-exclude:vendor src/
```

#### `--include:pattern`

Only format files matching this regex pattern. Can be specified multiple times.
Default is `\.nim(s|ble)?$`.

```sh
# Only format .nim files (not .nims or .nimble)
nph --include:'\.nim$' src/
```

**Note**: Files passed explicitly on the command line bypass all filtering
(matching Black's behavior):

```sh
# This WILL format vendor/foo.nim even if vendor is excluded
nph vendor/foo.nim
```

### Diff and check options

#### `--diff`

Show a unified diff of formatting changes without modifying files. Useful for
previewing changes or in CI to see what would be reformatted.

```sh
nph --diff src/myfile.nim
```

Exit code is 0 even if changes are found (informational mode). Combine with
`--check` to make it fail on changes.

#### `--check`

Check if files are formatted correctly without modifying them. Exits with code 1
if any file would be reformatted. Perfect for CI pipelines.

```sh
nph --check src/
```

Can be combined with `--diff` to both show changes and fail:

```sh
nph --check --diff src/
```

#### `--color` / `--no-color`

Enable or disable colored diff output. Only works with `--diff`. Default is
`--no-color`.

```sh
# Show colored diff
nph --diff --color src/myfile.nim

# Explicitly disable color
nph --diff --no-color src/myfile.nim
```

### Other options

#### `--config:file`

Specify a config file to use. Default is `.nph.toml` if it exists.

```sh
nph --config:custom.toml src/
```

## Disabling formatting locally

You can mark a code section with `#!fmt: off` and `#!fmt: on` to disable formatting locally:
```nim
proc      getsFormatted(a, b : int    ) = discard

#!fmt: off
let
  myHandFormattedList
        :
   array[3, int]
 =
    [1, 2, 3]

#!fmt: on
proc hanging(indent: int,
             isUgly = true) = discard
```

To disable formatting for a whole file, simply put `#!fmt: off` on top!

```admonish note
*Note* Internally, `#!fmt: off` makes nph treat the section as a big multi-line
comment that it copies over to the formatted code - as such, you must be careful
with indent and adjust your code to the indent that `nph` will generate!
```
