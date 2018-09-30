{ pkgs ? import ./pkgs.nix {} }:
pkgs.stdenv.lib.overrideDerivation (import ./. {inherit pkgs;}) (x: {
  nativeBuildInputs = x.nativeBuildInputs ++ [
    pkgs.cabal-install
  ];
})
