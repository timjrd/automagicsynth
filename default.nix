{ pkgs ? import ./pkgs.nix }:
with import pkgs {};
(haskellPackages.developPackage {
  root = ./.;
}).overrideAttrs (_:{
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
})
