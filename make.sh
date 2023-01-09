#!/bin/bash
# utility script for building the project
npm run build # or bsb -make-world
status=$?
if test $status -eq 0
then
  rm -rf dist
  parcel build src/index.html src/*csv --public-url ./
fi

