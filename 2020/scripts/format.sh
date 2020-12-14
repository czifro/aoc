#!/bin/sh

echo "Formatting files..."
haskell_files="$(ls app/*.hs app/**/*.hs src/*.hs src/**/*.hs test/*.hs test/**/*.hs *.hs | xargs)"

for hs in $haskell_files;
do
  brittany -c $hs

  if [ $? -ne 0 ]; then
    echo "Fixing format: '$hs'"
    brittany --indent=2 --write-mode=inplace $hs
  fi
done
