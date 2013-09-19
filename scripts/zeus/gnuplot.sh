#!/bin/bash
#PBS -A plgkrzywic2013b
#PBS -N emas-gnuplot
#PBS -k n
#PBS -l mem=256mb
#PBS -l nodes=1:ppn=1



model=$outputPath
modelName=$(basename $model)

mkdir -p $PlotRoot

fitness=()
population=()
for f in $model/*/*; do
	fitness+=("\"$f/fitnessMaxAvg.txt\" using (-\$1) with lines title \"cores=$(basename $(dirname $f)), islands=$(basename $f)\"")
	population+=("\"$f/populationSumAvg.txt\"  with lines title \"cores=$(basename $(dirname $f)), islands=$(basename $f)\"")
done
fitness=$( IFS=$','; echo "${fitness[*]}" )
fitness=${fitness%.}
population=$( IFS=$','; echo "${population[*]}" )
population=${population%.}


gnuplot -persist <<PLOT

set terminal svg size 1024 768
set output "$PlotRoot/fitness-$modelName.svg"

set title "fitnessMaxAvg - $modelName"
set key outside
set logscale y

plot $fitness 

quit
PLOT

gnuplot -persist <<PLOT

set terminal svg size 1024 768
set output "$PlotRoot/population-$modelName.svg"

set title "populationSumAvg - $modelName"
set key outside

plot $population 

quit
PLOT
