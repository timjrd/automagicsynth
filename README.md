# automagicsynth
*A generative music software with a custom synthesizer*

## Setup
Install the [Nix package manager](https://nixos.org/nix/):
```
curl https://nixos.org/nix/install | sh
```
... or install [GHC and Cabal](https://www.haskell.org/downloads#minimal).

## Build
Do an incremental build with Nix and Cabal:
```
nix-shell --pure --run "cabal build"
```

... or a full build with Nix:
```
nix-build
```

... or directly a Cabal build:
```
cabal build
```

## Run
The `./play` script contains parameters for SoX's `play` and VideoLAN's `cvlc`.
```
./dist/build/automagicsynth/automagicsynth | ./play
```
or
```
./result/bin/automagicsynth | ./play
```
