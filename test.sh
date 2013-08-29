#/bin/bash

## ilosc wykonanych testow dla kazdej konfiguracji
N=3
## dlugosc wektora problemu
Problem=10
## czas obliczania
Time=100
## ilosc rdzeni
TotalCores=`cat /proc/cpuinfo | grep processor | wc -l`

function concurrent {
   for (( i=0; i<N; i++ ))
   do
    Path=$4"/instance"$i
    mkdir -p $Path
   	erl -noshell -run concurrent run $1 $2 $3 $Path -run init stop
   done
}
function hybrid {
   for (( i=0; i<N; i++ ))
   do
    Path=$4"/instance"$i
    mkdir -p $Path
   	erl -noshell -run hybrid run $1 $2 $3 $Path -run init stop
   done
}
function sequential {
   for (( i=0; i<N; i++ ))
   do
    Path=$4"/instance"$i
    mkdir -p $Path
   	erl -noshell -run sequential run $1 $2 $3 $Path -run init stop
   	(( seqInstance++ ))
   done
}
function setCores {
    for (( core=0; core<$1 && core<TotalCores; core++ ))
    do
        #turning on the core
        echo 1 > /sys/devices/system/cpu/cpu${core}/online
    done
    for (( core=$1; core<TotalCores; core++ ))
    do
        #turning off the core
        echo 0 > /sys/devices/system/cpu/cpu${core}/online
    done
}
cd bin
for (( cores=2; cores<=8; cores = cores*2 ))
do
    #setCores $cores
    for (( island=2; island<=8; island = island*2 ))
    do
        concurrent $Problem $Time $island "Concurrent/"$island"_"$cores
        sequential $Problem $Time $island "Sequential/"$island"_"$cores
        hybrid $Problem $Time $island "Hybrid/"$island"_"$cores
    done
done
## na koniec wlacz wszystkie rdzenie
#setCores TotalCores