{ pkgs ? import ./pkgs.nix }:
with import pkgs {};

let hsPackages = haskellPackages.override {
  overrides = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      enableExecutableProfiling = true;
      enableLibraryProfiling    = true;
      doCheck = false;
    });
  };
};

in (hsPackages.developPackage {
  root = ./.;
}).overrideAttrs (_:{
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
})
