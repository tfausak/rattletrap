#!/usr/bin/env sh
set -o errexit -o xtrace

  URL="https://github.com/commercialhaskell/stack/releases/download/v1.9.3/stack-1.9.3-$TRAVIS_OS_NAME-x86_64.tar.gz"
  curl --location "$URL" --output stack.tar.gz
  gunzip stack.tar.gz
  tar -x -f stack.tar --strip-components 1
  mkdir -p "$HOME/.local/bin"
  mv stack "$HOME/.local/bin/"
  rm stack.tar

stack --version
