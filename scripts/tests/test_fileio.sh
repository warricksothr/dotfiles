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
mkdir -p tmp
cd tmp
touch $logfile
$bench --test=fileio --file-test-mode=seqwr run | tee -ai $logfile
free && sync && sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches' && free
$bench --test=fileio --file-test-mode=seqrd run | tee -ai $logfile
rm -f test_file.*
#$bench --test=fileio --file-test-mode=rndwr run
#$bench --test=fileio --file-test-mode=rndrd run
#rm -f test_file.*
cd ..
rm -rf tmp
