#!/bin/sh

(for i in `seq 1 10000`; do echo "http://localhost:7100/test/$i"; done ) > /tmp/mochi-urls.txt
