#!/usr/bin/env bash

if [ -z $1 ]; then 
    procs=1
else
    procs=$1
fi

bench_exec="sysbench"
bench_var="--num-threads=$procs"
bench="$bench_exec $benc_var"

$bench --test=cpu --cpu-max-prime=20000 run
