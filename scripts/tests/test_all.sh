#!/usr/bin/env bash

if [ -z $1 ]; then 
    procs=1
else
    procs=$1
fi

. test_cpu.sh
. test_threads.sh
. test_mem.sh
. test_fileio.sh
