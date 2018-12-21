#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash
runhaskell Setup.hs configure
runhaskell Setup.hs build
