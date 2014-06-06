#!/usr/bin/env bash

output_root=`./scripts/get_params.erl`


# rtime=120000
rtime=10000
cores="1 2 4"
run_repeat=3
# models="sequential hybrid"
models="concurrent"

rebar clean
rebar compile -D nif

for run in `seq 1 $run_repeat`; do
    for model in $models; do
        for core in $cores; do
            # nif ops
            mkdir -p $output_root/$model/$core/nif

            echo "running $model in $rtime mlsecs with nif on $core cores.."
            logfile="emas_$rtime"`date +"-%s"`".log"
            # echo $logfile
            output_file=$output_root/$model/$core/nif/$logfile
            erl +S 4:$core -pa ebin -run emas starts $model $rtime -run init stop -noshell > $output_file
       done
    done
done

rebar clean
rebar compile

for run in `seq 1 $run_repeat`; do
    for model in $models; do
        for core in $cores; do
            mkdir -p $output_root/$model/$core/erl

            logfile="emas_$rtime"`date +"-%s"`".log"
            output_file=$output_root/$model/$core/erl/$logfile

            echo "running $model in $rtime mlsecs on $core cores.."
            erl +S 4:$core -pa ebin -run emas starts $model $rtime -run init stop -noshell > $output_file
        done
    done
done