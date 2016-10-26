#!/usr/bin/env sh
set -o errxit -o xtrace

if test ! -f "$HOME/.local/bin/stack"
then
  URL="https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64"
  curl --location "$URL" --output stack.tar.gz
  gunzip stack.tar.gz
  tar -x -f stack.tar --strip-components 1
  mkdir -p "$HOME/.local/bin"
  mv stack "$HOME/.local/bin/"
  rm stack.tar
fi

stack --version
