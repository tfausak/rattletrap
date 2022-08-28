#! /usr/bin/env sh
set -o errexit -o xtrace

cabal update

test -f cabal.project.local ||
  cabal configure --enable-tests --jobs
