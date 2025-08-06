#!/usr/bin/env bash

#
# Usage: . init.sh
#

build="${build:?}"
project="${project:?}"

# FUNCTIONS

check-scripts () {
  shellcheck --norc --shell bash "$project/bin/"* "$project/init.sh"
  #
  # --no-rc = Don't look for .shellcheckrc files
  # --shell = Specify dialect (sh, bash, dash, ksh, busybox)
  #
}

format () {
  (cd "$project" && elm-format examples src tests "${@:---yes}")
}

preview () {
  (cd "$project" && elm-doc-preview "$@")
}

test () {
  (cd "$project" && elm-test "$@")
}

lint-examples () {
  (cd "$project/examples" && elm-test make src)
}

test-examples () {
  (cd "$project/examples" && elm-test "$@")
}

build-examples () {
  (cd "$project/examples" && pnpm build)
}

deploy-examples () {
  deploy "$build" release/examples/production
}

export -f \
  check-scripts \
  format preview test \
  lint-examples test-examples \
  build-examples deploy-examples

# ALIASES

alias c=check
alias f=format
alias p=preview
alias t=test
