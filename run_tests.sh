#!/usr/bin/env sh

run () {

    for run in `seq 1 $run_repeat`; do
        for model in $models; do
            for core in $cores; do
                mkdir -p $output_root/$model/$core/$1

                echo "running $model in $rtime mlsecs with $1 on $core cores.."
                logfile="emas_$rtime"`date +"-%s"`".log"

                output_file=$output_root/$model/$core/$1/$logfile
                echo $output_file
                erl +S 4:$core -pa ebin -pa deps/*/ebin \
                    -run emas starts $model $rtime \
                    -run init stop -noshell   > $output_file

            done
        done
    done
}



output_dir="output"
# rtime=120000
rtime=2000

# cores="1 2 4"
cores="4"
# run_repeat=3
run_repeat=1
models="skel_main" # "sequential"
# models="hybrid"

output_root=$output_dir/tests

./rebar compile


run nifops

