# Installation

## Binaries

Download binaries from the [releases page](https://github.com/arnetheduck/nph/releases/tag/latest).

## Install via nimble

`nph` can be compiled or installed using `nimble` v0.16.4+:

```sh
# Install globally
nimble install nph

# Alternatively, clone and build:
git clone https://github.com/arnetheduck/nph.git
cd nph
nimble setup -l
nimble build
```

```admonish note "Nim version"
`nph` requires an specific version of `nim` during the build process since it
reuses parts of the compiler whose API frequently changes - this may lead to
`nim` itself being built as part of the installation process!
```

For bonus points, replace `nimpretty` with a symlink to `nph` - similar
command line options are supported ;)

## Editor integration

* [VSCode](https://marketplace.visualstudio.com/items?itemName=NimLang.nimlang) (`ext install NimLang.nimlang`) extension via [nimlangserver](https://github.com/nim-lang/langserver/) that supports `nph` out of the box
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
* [vscode-nph](https://marketplace.visualstudio.com/items?itemName=arnetheduck.vscode-nph) (`ext install arnetheduck.vscode-nph`) for a formatting-only option for the official Nim extension.

## Continuous integration

Check out the [companion Github Action](https://github.com/arnetheduck/nph-action) for a convenient CI option!

