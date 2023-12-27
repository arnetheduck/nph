# Installation

## Binaries

Download binaries from the [releases page](https://github.com/arnetheduck/nph/releases/tag/latest).

## Install via nimble

`nph` can be also compiled or installed using `nimble` version `v0.14.2`+:

```sh
nimble -l setup
nimble build
```

```admonish note "Nim version requirement"
`nph` requires an specific version of `nim` during the build process since it
reuses parts of the compiler whose API frequently changes - this may lead to
`nim` itself being built as part of the installation process
```

For bonus points, replace `nimpretty` with a symlink to `nph` - similar
command line options are supported ;)

## Editor integration

* [VSCode](https://marketplace.visualstudio.com/items?itemName=arnetheduck.vscode-nph) (`ext install arnetheduck.vscode-nph`)
