#!/bin/bash
# Helper script that formats all Nim files in a repository and commits the changes

set -eo pipefail

git diff --exit-code > /dev/null || { echo "Commit changes before doing a (potentially) large reformat!" ; exit 1 ; }

git ls-files | grep ".nim$" | xargs -n1 nph

! git diff --exit-code > /dev/null || { echo "This repository is already formatted" ; exit 0 ; }

git commit -am "Formatted with nph $(nph --version)"

echo "# Formatted with nph $(nph --version)" >> .git-blame-ignore-revs
echo "$(git rev-parse HEAD)" >> .git-blame-ignore-revs

git add .git-blame-ignore-revs
git commit -m "Add $(git rev-parse HEAD) to .git-blame-ignore-revs"

echo "The repo has been updated with two commits recording the reformatting and blame information."
echo "You can review the changes with 'git diff HEAD^^' before pushing to a public repository."
echo "If you don't want blame information, remove the last commit with 'git reset --hard HEAD^'."

