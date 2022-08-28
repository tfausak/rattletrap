#! /usr/bin/env sh
set -o errexit -o xtrace

volume=cabal-store
docker volume inspect "$volume" ||
  docker volume create "$volume"
