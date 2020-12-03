#!/bin/sh

nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [hpack cabal2nix])" \
          --run "hpack && cabal2nix . > default.nix"
