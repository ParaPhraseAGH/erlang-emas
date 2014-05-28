#!/usr/bin/env bash

out_rebar_conf () {
# (logdir, nif,    # model, runtime, )
rebar_out="rebar.config"
echo "" > $rebar_out
echo "%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

%% == Erlang Compiler ==

%% Erlang compiler options
{erl_opts, [debug_info,
            warn_missing_spec," >> $rebar_out
# echo \
# "            {d, logdir, $1}," >> $rebar_out
echo \
"            {d, probsize, $2}," >> $rebar_out
if [ -n "$3" ]; then
    echo \
"            {d, nif}," >> $rebar_out
fi
           # % {d, nif},

echo \
"            {src_dirs, [\"src\"]}]}.

%% == EDoc ==

%% EDoc options
{edoc_opts, []}.

%% == Cleanup ==

%% Which files to cleanup
{clean_files, [\"*.beam\"]}.

%% == Dependencies ==

%% Where to put any downloaded dependencies. Default is \"deps\"
{deps_dir, \"deps\"}.

%% What dependencies we have, dependencies can be of 3 forms, an application
%% name as an atom, eg. mochiweb, a name and a version (from the .app file), or
%% an application name, a version and the SCM details on how to fetch it (SCM
%% type, location and revision).
%% Rebar currently supports git, hg, bzr, svn, and rsync.
{deps, [
        {skel, \".*\",
        {git, \"git://github.com/ParaPhrase/skel.git\", {branch, \"master\"}}}
       ]
}.

{port_specs, 
    [
        {\"priv/rastrigin_nif.so\", [\"c_src/rastrigin*.c\"]}
    ]
}." >> $rebar_out

}


output_root=`./scripts/get_params.erl`


# rtime=120000
rtime=120000
cores="1 2 4"
run_repeat=3
models="sequential hybrid"
rebar clean
rebar compile -D nif

for run in `seq 1 $run_repeat`; do
    for model in $models; do
        for core in $cores; do
            # nif ops
            # out_rebar_conf 0 100 nif
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