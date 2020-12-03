#!/bin/sh

nix-shell --run 'cabal new-build --enable-tests'
