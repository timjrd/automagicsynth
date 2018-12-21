{ pkgs ? import ./pkgs.nix }:
with pkgs;

let hsPackages = haskellPackages.override {
  overrides = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      # enableExecutableProfiling = true;
      # enableLibraryProfiling    = true;
      # doCheck = false;
    });
  };
};

in (hsPackages.developPackage {
  root = ./.;
}).overrideAttrs (x: {
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;
  buildInputs = x.buildInputs ++ [
    vlc
    ffmpeg
  ];
})
