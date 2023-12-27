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

# You can format stuff as part of a pipe using `-` as input:
echo "echo 1" | nph -
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
