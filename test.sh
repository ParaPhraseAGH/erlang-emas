#/bin/bash

## ilosc wykonanych testow dla kazdej konfiguracji
N=2
## dlugosc wektora problemu
Problem=10
## czas obliczania
Time=100
## ilosc rdzeni
TotalCores=`cat /proc/cpuinfo | grep processor | wc -l`

function runModel {
   for (( i=0; i<N; i++ )) do
    Path=$6"/instance"$i
    mkdir -p $Path
   	taskset $1 erl -noshell -run $2 run $3 $4 $5 $Path -run init stop
   done
}

cd bin
for cores in 0x00000010 0x00000170 0x00000255 do
    for (( island=2; island<=8; island = island*2 )) do
        runModel $cores concurrent $Problem $Time $island "Concurrent/"$island"_"$cores
        runModel $cores sequential $Problem $Time $island "Sequential/"$island"_"$cores
        runModel $cores hybrid $Problem $Time $island "Hybrid/"$island"_"$cores
    done
done