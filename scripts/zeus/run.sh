#/bin/bash

export EmasRoot="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
export OutputRoot=~/test #output
export PlotRoot=~/plots
Scripts=$EmasRoot/scripts/zeus

## ilosc wykonanych testow dla kazdej konfiguracji
export N=1
## dlugosc wektora problemu
export Problem=100
## czas obliczania
export Time=6000
## Topologia wysp
export Topology=mesh

Models=(sequential) # hybrid concurrent)
Cores=(1) # 2 4)
Islands=(1) # 2 4)

lock=`qsub $Scripts/noop.sh -h`

jobs=()
for model in ${Models[*]}; do
    export model
    statJobs=()
    for cores in ${Cores[*]}; do
	export cores
        for islands in ${Islands[*]}; do
	   export islands
	   export outputPath=$OutputRoot/$model/$cores/$islands
	   emasJobs=`qsub $Scripts/emas.sh -l cput=$(($Time / 1000)),nodes=1:ppn=$cores -V -t 1-$N -W depend=afterok:$lock`
	   statJobs+=(`qsub $Scripts/extractor.sh -V -W depend=afterokarray:$emasJobs`)
        done
    done
    export outputPath=$OutputRoot/$model
    qsub $Scripts/gnuplot.sh -V -W depend=afterok$(printf ":%s" "${statJobs[@]}") -m e -M krzywic@agh.edu.pl -z
done

qrls $lock
