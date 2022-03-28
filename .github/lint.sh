#!/bin/bash

SOURCES=$(find ~+ -maxdepth 1 -type d -name '*plutus*')

for project in ${SOURCES}
do
  cd "${project}" && hlint $(find ~+ -name '*.hs' -not -path './dist-*/*') && cd ..
done
