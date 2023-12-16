#!/bin/bash
# Helper script that formats all Nim files in a repository and commits the changes

set -eo pipefail

git ls-files | grep ".nim$" | xargs -n1 nph

git commit -am "Formatted with nph $(nph --version)"

echo "Add the following two lines to .git-blame-ignore-revs to maintain git blame:"
echo "# Formatted with nph $(nph --version)"
echo "$(git rev-parse HEAD)"