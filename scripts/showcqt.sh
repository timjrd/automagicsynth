#!/usr/bin/env bash
ffmpeg -f s16le -ar 44100 -ac 2 -i - \
       -codec:audio libmp3lame -qscale:audio 0 \
       -filter_complex "showcqt=size=1080x1080:text=0" \
       "$1"
