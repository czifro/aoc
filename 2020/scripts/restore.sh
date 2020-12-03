#!/bin/sh

nix-shell --run 'cabal update && echo "Restored Dependencies"'
