# Package

version       = "0.3"
author        = "Jacek Sieka"
description   = "Nim code formatter"
license       = "MIT"
srcDir        = "src"
bin           = @["nph"]

# Dependencies

# TODO https://github.com/nim-lang/nimble/issues/1166
# Using exact version here and adding path manually :facepalm:
# run `nimble setup -l` to hopefully make it work
requires "nim >= 2.0.0",
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


proc formatProject(
  name, url, branch: string, dirs: openArray[string]
) =
  if not dirExists("playground"):
    mkdir("playground")
  cd "playground/"
  if not dirExists(name):
    exec "git clone --single-branch --branch " & branch & " " & url & " " & name

  cd name
  for dir in dirs:
    if dir.len > 0:
      cd dir
    exec "git checkout " & branch & " -- ."
    exec "git restore --staged ."
    try:
      exec "git ls-files | grep .nim$ | xargs nph"
      exec "git diff"
    except: discard
    if dir.len > 0:
      cd ".."
  cd "../.."

proc commitProject(
  name, url, branch: string, dirs: openArray[string]
) =
  formatProject(name, url, branch, dirs)

  cd "playground/" & name

  try:
    exec "git checkout -b nph"
    exec "git commit -am \"Formatted with nph $(nph --version)\""
  except:
    exec "git checkout nph"
    exec "git commit -am \"Formatted with nph $(nph --version)\" --amend"

  cd "../.."

const projects = [
  ("Nim", "https://github.com/arnetheduck/Nim.git", "version-2-0", @["compiler", "lib"]),
  ("nim-results", "https://github.com/arnetheduck/nim-results.git", "master", @[""]),
  ("nimbus-eth2", "https://github.com/status-im/nimbus-eth2.git", "unstable", @[""]),
]

task play, "Format several popular projects":
  for p in projects:
    formatProject(p[0], p[1], p[2], p[3])

task replay, "Commit formatted sources":
  for p in projects:
    commitProject(p[0], p[1], p[2], p[3])
