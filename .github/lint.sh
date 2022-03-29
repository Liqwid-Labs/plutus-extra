#!/bin/bash

SOURCES=$(find ~+ -maxdepth 1 -type d -name '*plutus*')

for project in ${SOURCES}
do
  if ! (cd "${project}"; hlint $(find ~+ -name '*.hs' -not -path './dist-*/*'))
  then
    echo "Linting issues found, aborting..."
    exit 1
  fi
done
