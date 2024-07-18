# nph

`nph` is an opinionated source code formatter for the Nim language, aiming to
take the drudgery of manual formatting out of your coding day.

Following the great tradition of [`black`](https://github.com/psf/black/),
[`prettier`](https://prettier.io/), [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html)
and other AST-based formatters, it discards existing styling to create a
consistent and beautiful codebase.

## Documentation

Documentation is available [here](https://arnetheduck.github.io/nph/).

## Quickstart

Install `nph`, then run it on some files:

```sh
# Format the given files in-place
nph file0.nim file1.nim

# Format the given files, writing the formatted code to /tmp
nph file0.nim file1.nim --outdir:/tmp

# Format an entire directory
nph src/

# Use --check to verify that a file is formatted as `nph` would - useful in CI
nph --check somefile.nim || echo "Not formatted!"

# You can format stuff as part of a pipe using `-` as input:
echo "echo 1" | nph -
```

More information about features and style available from the [documentation](https://arnetheduck.github.io/nph/)

## Installation

Download binaries from the [releases page](https://github.com/arnetheduck/nph/releases/tag/latest).

`nph` can be also compiled or installed using `nimble` version `v0.14.2`+:

```sh
nimble -l setup
nimble build
```

## Editor integration

* [VSCode](https://marketplace.visualstudio.com/items?itemName=arnetheduck.vscode-nph) (`ext install arnetheduck.vscode-nph`)
* [NeoVim](https://github.com/sbdchd/neoformat) - Install **neoformat** in your neovim setup then add the nim formating option with **nph** with this option in init.vim `let g:neoformat_enabled_nim = ['nph']`
* [Zed Editor](https://github.com/foxoman/zed-nim) - Use this in your editor settings 
```
"languages": {
    "Nim": {
      "formatter": {
        "external": {
          "command": "nph",
          "arguments": ["-"]
        }
      }
    }
  }
```
