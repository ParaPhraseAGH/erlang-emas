#!/usr/bin/env sh

run () {

    for run in `seq 1 $run_repeat`; do
        for model in $models; do
            for core in $cores; do
                for workers in $skel_workers; do
                    mkdir -p $output_root/$model/$core/w$workers

                    echo "running $model in $rtime mlsecs with $workers skel workers on $core cores.."
                    logfile="emas_$rtime"`date +"-%s"`".log"

                    output_file=$output_root/$model/$core/w$workers/$logfile
                    echo $output_file
                    erl +S 4:$core -pa ebin -pa deps/*/ebin \
                        -eval "emas:start($model,$rtime,[{skel_workers,$workers},{genetic_ops, emas_test_ops}])." \
                        -run init stop -noshell
                    #> $output_file
                done
            done
        done
    done
}


output_dir="output"
# rtime=120000
rtime=3000

# cores="1 2 4"
cores="4"
# run_repeat=3
run_repeat=1
skel_workers=4
models="mas_skel mas_sequential mas_concurrent mas_hybrid"

output_root=$output_dir/tests

run

