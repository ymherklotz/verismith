#!/bin/sh

if [[ -z $NIX ]]; then
  if [[ $NIX -eq 0 ]]; then
    cabal update
    cabal build
  else
    nix-build
  fi
fi

