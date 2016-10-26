#!/usr/bin/env sh
set -o errexit -o xtrace

if test ! -f "$HOME/.local/bin/github-release"
then
  VERSION=0.1.9
  URL="https://github.com/tfausak/github-release/releases/download/$VERSION/github-release-$VERSION-$TRAVIS_OS_NAME.gz"
  curl --location "$URL" --output github-release.gz
  gunzip github-release.gz
  chmod u+x github-release
  mkdir -p "$HOME/.local/bin"
  mv github-release "$HOME/.local/bin/"
fi

github-release version
