#!/usr/bin/env bash

if test $(command -v nix-shell)
then
    nix-shell --pure --run "
      runhaskell Setup.hs configure
      runhaskell Setup.hs build
    "
else
    echo '"nix-shell" not found.'
fi
