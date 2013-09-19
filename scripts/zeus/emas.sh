#/bin/bash
#PBS -l mem=256mb
#PBS -A plgkrzywic2013b 
#PBS -N emas
#PBS -k n

outputPath=$outputPath/$PBS_ARRAYID
mkdir -p $outputPath

module load erlang
erl -pa $EmasRoot/ebin -noshell -run $model start $Problem $Time $islands $Topology $outputPath -run init stop

java -jar $EmasRoot/stat-extractor.jar $outputPath fitness:max population:sum
