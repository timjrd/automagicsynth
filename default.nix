{ nixpkgs ? null }: with builtins; let
  zeros = "0000000000000000000000000000000000000000000000000000";
  github = {repo, rev, sha256 ? zeros}: fetchTarball {
    url = "https://github.com/${repo}/archive/${rev}.tar.gz";
    inherit sha256;
  };
  
  nixpkgs' = github {
    repo = "NixOS/nixpkgs-channels";
    rev = "c06f5302f63af92c9ba0e10401f366e0c7bd7d49";
    sha256 = "01fnr797a6iv3cy8gmddmm5gdlrijjdm66y9v14jai3np8fx1nyz";
  };
    
  nixpkgs'' = if nixpkgs != null then nixpkgs else nixpkgs';
in

with import nixpkgs'' {};
with lib;

let hsPackages = haskellPackages.override {
  overrides = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      enableExecutableProfiling = true;
      # enableLibraryProfiling = true;
      doCheck = false;
    });
  };
};

in (hsPackages.developPackage {
  root = ./.;
}).overrideAttrs (x: {
  src = filterSource (path: _: ! any (x: hasPrefix x (baseNameOf path)) [
    "build.sh"
    "default.nix"
    "dist"
    ".git"
    "README.md"
    "records"
    "repl.sh"
    "result"
    "run.sh"
    "scripts"
  ]) ./.;
  
  buildInputs = x.buildInputs ++ [
    vlc
    ffmpeg
    gnuplot
  ];
})
