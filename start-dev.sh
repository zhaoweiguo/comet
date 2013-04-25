#!/bin/sh

cd `dirname $0`
exec erl -pa ./apps/*/ebin -pa ./deps/*/ebin -boot start_sasl -args_file "etc/vm.args" -config "etc/app.config" -s reloader



