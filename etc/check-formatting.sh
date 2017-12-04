#!/bin/bash

# Check that the existing formatting of Haskell sources matches the one of
# hindent and stylish-haskell combined (in that order).

set -e

HS_FILES="$(find codegen -name '*.hs')"

for file in $HS_FILES ; do
  echo "Format checking $file"
  diff --color -u <(cat $file | hindent -XPatternSynonyms | stylish-haskell) <(cat $file)
done
