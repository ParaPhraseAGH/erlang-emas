#!/bin/sh

EmasRoot="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../.. && pwd )"
OutputRoot=output

# Computation settings
N=1			# Number of runs for each computation
Time=6000       	# Computation time [ms]
Problem=100		# Problem size	
Islands=12		# Islands amount
Topology=mesh		# Islands topology

# Common Zeus settings
CommonSettings="-t 1-$N"				# Number of runs for each job
CommonSettings+=" ""-l mem=1gb"				# Available memory
CommonSettings+=" ""-l file=500mb"			# Available disc
CommonSettings+=" ""-l walltime=$(($Time / 800))"	# Available time [s] (~120% predicted computation time)
CommonSettings+=" ""-j oe"				# Join stdout and stderr
CommonSettings+=" ""-A plgkrzywic2013b"			# Grant ID
CommonSettings+=" ""-N emas"				# Job name

# Independent variables
Models=(sequential_lists) # hybrid concurrent)
Cores=(1 2 4 8 12)

for model in ${Models[*]}; do
    for cores in ${Cores[*]}; do

	Command="module load erlang\n"
	Command+="erl -pa $EmasRoot/ebin -noshell -run $model start $Problem $Time $Islands $Topology -run init stop"

	outputPath=$OutputRoot/$model/$cores
	mkdir -p $outputPath

	Settings=$CommonSettings		
	Settings+=" ""-l nodes=1:X5650:ppn=$cores"	# Available cores
	Settings+=" ""-o $outputPath"			# Output directory
	
	echo -e "$Command" | qsub $Settings
    done
done

