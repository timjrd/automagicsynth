#! /usr/bin/env bash
if test $(command -v cvlc)
then
    cvlc --demux=rawaud \
	 --rawaud-channels 2 \
	 --rawaud-samplerate 44100 \
	 --file-caching 4000 -
else
    echo '"cvlc" not found.'
fi
