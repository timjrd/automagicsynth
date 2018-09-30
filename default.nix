{ pkgs ? import ./pkgs.nix {} }:
pkgs.haskellPackages.developPackage {
  root = ./.;
}
