# Testbench

The testbench of `nph` is split into two parts:

* before/after tests in `tests/` - these cover the basics as well as an assortment
  of unusually formatted code in the wild - changes and additions to the
  formatting rules usually find representation here
* playground - formatting of several large-ish projects

## Tests

Unit tests with before/after stored in the repo and checked by CI - should
be extended with each proposal to change the formatting rules:
```sh
# Run the tests
nimble f
```

## Playground

The playground can be used to test two important scenarios: formatting a "raw"
hand-formatted codebase and re-formatting a codebase already formatted by `nph`.

```sh
# Format an existing codebase and show the diff against the upstream version -
# this will clone a clean version of the project and format it with `nph`
nimble play

# Same as above, but commit the changes to establish a starting point for making
# changes to the formatting algo
nimble replay

# Format the current state of the playground without resetting the repository -
# together with `replay`, it allows checking the diff of a suggested formatting
# change - oddities found here should be brought back to the regular test suite
nimble again
```
