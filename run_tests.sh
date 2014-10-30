#!/usr/bin/env sh

run () {

    for model in $models; do
        for core in $cores; do
            for split_size in $split_sizes; do
                for ops in $operators; do
                    for skel_pull in $skel_pull_workflow; do
                    for run in `seq 1 $run_repeat`; do
                        mkdir -p $output_root/$ops/$model/$core/w$workers

                        echo "running $model in $rtime mlsecs with $workers skel workers on $core cores with $ops operators.."
                        logfile="emas_$rtime"`date +"-%s"`".log"

                        output_file=$output_root/$ops/$model/$core/w$workers/$logfile
                        echo $output_file
                        command="emas:start($model,$rtime,[{skel_workers,4},{genetic_ops,$ops},{problem_size,30}],[{split_size,$split_size},{skel_pull,$skel_pull}])."
                        echo $command
                        erl +S 4:$core -pa ebin -pa deps/*/ebin \
                            -eval $command \
                            -run init stop -noshell
                        #> $output_file
                    done
                    done
                done
            done
        done
    done
}


output_dir="output"
# rtime=120000
rtime=5000

# cores="1 2 4"
cores="4"
# run_repeat=3
run_repeat=1
skel_pull_workflow="enable disable"
split_sizes="2 8 20 30 40 50 100"
models="mas_skel"
#" mas_sequential mas_concurrent mas_hybrid"
operators="emas_test_ops"


output_root=$output_dir/tests

run
