#/bin/bash

## ilosc wykonanych testow dla kazdej konfiguracji
N=30
## dlugosc wektora problemu
Problem=50
## czas obliczania
Time=60000
## ilosc rdzeni
TotalCores=`cat /proc/cpuinfo | grep processor | wc -l`

function runModel {
   for (( i=0; i<N; i++ )) do
    Path=$7"/instance"$i
    mkdir -p $Path
    echo $Path
   	taskset $1 erl -noshell -run $2 start $3 $4 $5 $6 $Path -run init stop
   done
}

cd ebin
for topology in mesh ring
do
    for cores in 0x0000000A 0x000000AA 0x000000FF
    do
        for island in 2 4 8
        do
            runModel $cores concurrent $Problem $Time $island $topology "Concurrent/"$island"_"$cores"_"$topology
            runModel $cores sequential $Problem $Time $island $topology "Sequential/"$island"_"$cores"_"$topology
            runModel $cores hybrid $Problem $Time $island $topology "Hybrid/"$island"_"$cores"_"$topology
        done
    done
done