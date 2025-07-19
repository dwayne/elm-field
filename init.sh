#!/usr/bin/env bash

#
# Usage: . init.sh
#

project="${project:?}"

# FUNCTIONS

format () {
  (cd "$project" && elm-format examples src tests "${@:---yes}")
}

preview () {
  (cd "$project" && elm-doc-preview "$@")
}

test () {
  (cd "$project" && elm-test "$@")
}

test-examples () {
  (cd "$project/examples" && elm-test "$@")
}

export -f format preview test test-examples

# ALIASES

alias c=check
alias f=format
alias p=preview
alias t=test
