#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname comet_dev \
    +K true  \  # enable kernel-poll
    +P 134217727 \   # default maximum number of processes you can spawn is 32768;  134,217,727 is the max according to "man erl"
    -s comet \
    -s reloader
