#!/usr/bin/env sh
set -o errexit -o xtrace

if test ! -f "$HOME/.local/bin/github-release"
then
  URL="https://github.com/tfausak/github-release/releases/download/1.1.0/github-release-1.1.0-$TRAVIS_OS_NAME.gz"
  curl --location "$URL" --output github-release.gz
  gunzip github-release.gz
  chmod +x github-release
  mkdir -p "$HOME/.local/bin"
  mv github-release "$HOME/.local/bin/"
fi

github-release version
