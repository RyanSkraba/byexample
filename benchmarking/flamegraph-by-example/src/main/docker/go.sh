#!/usr/bin/env bash

__tagMapping=${1:-org.application}
__prefix=${2:-org.Application}

rm -rf /opt/flamegraph/traces
python influxdb_dump.py -o localhost -u profiler -p profiler -d profiler -t $__tagMapping -e $__prefix -x /opt/flamegraph/traces > /dev/null
perl flamegraph.pl /opt/flamegraph/traces/all_*.txt