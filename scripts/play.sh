#!/usr/bin/env bash
cvlc --demux=rawaud \
     --rawaud-channels 2 \
     --rawaud-samplerate 44100 - \
