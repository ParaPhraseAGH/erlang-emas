#!/bin/sh

## ilosc wykonanych testow dla kazdej konfiguracji
N=2
## dlugosc wektora problemu
Problem=10
## czas obliczania
Time=2000
## ilosc rdzeni
TotalCores=`cat /proc/cpuinfo | grep processor | wc -l`

function runModel {
   for (( i=0; i<N; i++ )) do
    Path=$6"/instance"$i
    mkdir -p $Path
   	taskset $1 erl -noshell -run $2 run $3 $4 $5 $Path -run init stop
   done
}

cd ebin
for cores in 0x0000000A 0x000000AA 0x000000FF
do
    for island in 2 4 8
    do
        runModel $cores concurrent $Problem $Time $island "Concurrent/"$island"_"$cores
        runModel $cores sequential_lists $Problem $Time $island "Sequential/"$island"_"$cores
        runModel $cores sequential_mixed $Problem $Time $island "Sequential/"$island"_"$cores
        runModel $cores hybrid $Problem $Time $island "Hybrid/"$island"_"$cores
    done
done
