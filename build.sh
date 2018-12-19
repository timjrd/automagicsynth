#!/usr/bin/env bash

if test $(command -v nix-shell)
then
    nix-shell --pure --run "runhaskell Setup.hs configure && runhaskell Setup.hs build"
    
elif test $(command -v cabal)
then
    cabal new-build
else
    echo "Please install one of:"
    echo "1. Nix  : https://nixos.org/nix/"
    echo "2. Cabal: https://www.haskell.org/cabal/"
fi
