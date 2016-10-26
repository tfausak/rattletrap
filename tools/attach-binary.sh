#!/usr/bin/env sh
set -o errexit -o xtrace

if test -z "$TRAVIS_TAG"
then
  echo 'The $TRAVIS_TAG variable is empty.'
  exit
fi

if test -z "$GITHUB_TOKEN"
then
  echo 'The $GITHUB_TOKEN variable is empty.'
  exit
fi

BIN="$(stack path --local-install-root)/bin/rattletrap"
chmod +x "$BIN"
gzip --best --to-stdout "$BIN" > rattletrap.gz
github-release upload \
  --token "$GITHUB_TOKEN" \
  --owner tfausak \
  --repo rattletrap \
  --tag "$TRAVIS_TAG" \
  --file rattletrap.gz \
  --name "rattletrap-$TRAVIS_TAG-$TRAVIS_OS_NAME.gz"
