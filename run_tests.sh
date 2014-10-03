#!/usr/bin/env sh

run () {

    ./rebar clean
    ./rebar compile -D $1

    for run in `seq 1 $run_repeat`; do
        for model in $models; do
            for core in $cores; do
                mkdir -p $output_root/$model/$core/$1

                echo "running $model in $rtime mlsecs with $1 on $core cores.."
                logfile="emas_$rtime"`date +"-%s"`".log"

                output_file=$output_root/$model/$core/$1/$logfile
                echo $output_file
                erl +S 4:$core -pa ebin -run emas starts $model $rtime -run init stop -noshell > $output_file

            done
        done
    done
}

output_dir="output"
# rtime=120000
rtime=200

# cores="1 2 4"
cores="4"
# run_repeat=3
run_repeat=1
models="sequential" # "skel_main"
# models="hybrid"

output_root=$output_dir/tests


run nifops
run binops
run erlops
