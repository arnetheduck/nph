# Package

version       = "0.1"
author        = "Jacek Sieka"
description   = "Nim code formatter"
license       = "MIT"
srcDir        = "src"
bin           = @["nph"]

# Dependencies

# TODO https://github.com/nim-lang/nimble/issues/1166
# Using exact version here and adding path manually :facepalm:
# run `nimble setup -l` to hopefully make it work
requires "nim == 2.0.0",
         "compiler"

proc build() =
  exec "nim c --debuginfo -o:nph src/nph"

task self, "Format nph itself":
  build()

  # Require there are no changes before self-formatting! Can stage if needed
  exec "git diff --no-ext-diff --quiet --exit-code"

  for file in listFiles("src"):
    if file.len > 4 and file[^4..^1] == ".nim":
      echo file
      exec "./nph " & file

import std/algorithm
task f, "Format":
  build()

  cd "tests/before"
  # Sort tests so that 00_empty always is first, which makes it a convenient
  # experimentation ground :)
  for file in sorted(listFiles(".")):
    if file.len > 4 and file[^4..^1] == ".nim":
      echo file
      exec "../../nph " & file & " --outDir:../after --debug"
