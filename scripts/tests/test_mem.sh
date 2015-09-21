#!/usr/bin/env bash

if [ -z $1 ]; then 
    procs=1
else
    procs=$1
fi

bench_exec="sysbench"
bench_var="--num-threads=$procs"
bench="$bench_exec $benc_var"

machine_ident="$(uname -nmo | sed 's/ /_/g' | sed 's/\//\./g')_$(cat /var/lib/dbus/machine-id)"
logfile="$PWD/results/$(date -Idate)_${machine_ident}.log"
touch $logfile
$bench --test=mem run | tee -ai $logfile
