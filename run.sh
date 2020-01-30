#! /usr/bin/env nix-shell
#! nix-shell -i bash
bin=./dist/build/automagicsynth/automagicsynth
function run {
  mkdir -p records
  record=$(date +%Y-%m-%d-%H-%M-%S).pcm
  echo $record
  echo
  $bin | tee records/$record | ./scripts/play.sh
  # $bin +RTS -xc | tee records/$time.pcm | ./scripts/play.sh
}

if test -f $bin
then run
else ./build.sh && run
fi
