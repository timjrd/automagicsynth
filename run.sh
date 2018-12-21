#! /usr/bin/env nix-shell
#! nix-shell -i bash
bin=./dist/build/automagicsynth/automagicsynth
function run {
    $bin | ./scripts/play.sh
}

if test -f $bin
then run
else ./build.sh && run
fi
