#!/usr/bin/env sh

run () {

    for model in $models; do
        for core in $cores; do
            for split_size in $split_sizes; do
                for skel_pull in $skel_pull_workflow; do
                    for run in `seq 1 $run_repeat`; do
                        dir=$output_root/$core/$skel_pull/$split_size
                        mkdir -p $dir

                        echo "running $model in $rtime mlsecs with $workers skel workers on $core cores with $ops operators.."
                        logfile="emas_$rtime"`date +"-%s"`".log"

                        output_file=$dir/$logfile
                        ./emas --time $rtime \
                               --model $model \
                               --skel_workers 4 \
                               --genetic_ops emas_test_ops \
                               --problem_size 30 \
                               --skel_split_size $split_size \
                               --skel_pull $skel_pull || exit 1
                        #> $output_file
                    done
                done
            done
        done
    done
}


output_dir="output"
# rtime=120000
rtime=10000

# cores="1 2 4"
cores="4"
# run_repeat=3
run_repeat=1
skel_pull_workflow="enable"
split_sizes="2"
models="mas_sequential mas_skel mas_hybrid mas_concurrent"
#" "
operators="emas_test_ops"


output_root=$output_dir/tests

run
