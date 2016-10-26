#!/usr/bin/env sh
set -o errexit -o xtrace

if test -z "$TRAVIS_TAG"
then
  echo 'The $TRAVIS_TAG variable is empty.'
  exit
fi

if test "$TRAVIS_OS_NAME" != linux
then
  echo "The \$TRAVIS_OS_NAME is '$TRAVIS_OS_NAME' instead of 'linux'."
  exit
fi

mkdir -p "$HOME/.stack/upload"
echo "{ \"username\": \"$HACKAGE_USERNAME\", \"password\": \"$HACKAGE_PASSWORD\" }" > "$HOME/.stack/upload/credentials.json"
stack upload .
