#!/usr/bin/env bash

if [ -z $1 ]; then 
    procs=1
else
    procs=$1
fi

. test_cpu.sh $procs
. test_threads.sh $procs
. test_mem.sh $procs
. test_fileio.sh $procs
