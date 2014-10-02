#!/usr/bin/env sh

EmasRoot="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../.. && pwd )"
OutputDir=output
GrantID=plganiel2014a

# Computation settings
N=10			# Number of runs for each computation
Time=1800000       	# Computation time [ms]
# not configurable here
#Problem=100		# Problem size	
#Islands=12		# Islands amount
#Topology=mesh		# Islands topology

OpsModule=erlops
# recompile for new ops module
rm $EmasRoot/ebin/emas_config.beam
./rebar compile -D $OpsModule

OutputRoot=`$EmasRoot/scripts/get_params.erl $OutputDir`"_"`expr $Time / 1000`"s"
echo $OutputRoot

# Common Zeus settings
CommonSettings="-t 1-$N"				# Number of runs for each job
CommonSettings+=" ""-l mem=1gb"				# Available memory
CommonSettings+=" ""-l file=500mb"			# Available disc
CommonSettings+=" ""-l walltime=$(($Time / 800))"	# Available time [s] (~120% predicted computation time)
CommonSettings+=" ""-j oe"				# Join stdout and stderr
CommonSettings+=" ""-A $GrantID"			# Grant ID
CommonSettings+=" ""-N emas"				# Job name
# CommonSettings+=" ""-q l_bigmem"

# Independent variables
#Models=(hybrid concurrent skel_main)
Models=(sequential skel_main)
Cores=(1 2 4 8 12)
#Procesor=opteron6276
Procesor=X5650

for model in ${Models[*]}; do
    for cores in ${Cores[*]}; do

	Command="module load apps/erlang/17.3\n"
	# Command+="erl -pa $EmasRoot/ebin -pa $EmasRoot/deps/*/ebin -noshell -run $model start $Problem $Time $Islands $Topology standard_io -run init stop"
	Command+="erl -pa $EmasRoot/ebin -pa $EmasRoot/deps/*/ebin -noshell -eval 'emas:start($model,$Time).' -run init stop"	

	outputPath=$OutputRoot/$model/$cores/$OpsModule
	mkdir -p $outputPath

	Settings=$CommonSettings		
	Settings+=" ""-l nodes=1:$Procesor:ppn=$cores"	# Available cores
	Settings+=" ""-o $outputPath"			# Output directory
	
	echo -e "$Command" | qsub $Settings
    done
done

