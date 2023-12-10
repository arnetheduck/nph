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

  # Stage changes before doing self-formatting!
  exec "git diff --staged --no-ext-diff --quiet --exit-code"
  exec "git add -A"

  for file in listFiles("src"):
    if file.len > 4 and file[^4..^1] == ".nim":
      echo file
      exec "./nph " & file & " --debug"

task f, "Format":
  build()

  cd "tests/before"
  for file in listFiles("."):
    if file.len > 4 and file[^4..^1] == ".nim":
      echo file
      exec "../../nph " & file & " --outDir:../after --debug"
