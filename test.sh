#/bin/bash

## ilosc wykonanych testow dla kazdej konfiguracji
N=2
## dlugosc wektora problemu
Problem=10
## czas obliczania
Time=100
## ilosc rdzeni
TotalCores=`cat /proc/cpuinfo | grep processor | wc -l`

function concurrent {
   for (( i=0; i<N; i++ ))
   do
   	erl -noshell -run concurrent run $1 $2 $3 -run init stop
   done
}
function hybrid {
   for (( i=0; i<N; i++ ))
   do
   	erl -noshell -run hybrid run $1 $2 $3 -run init stop
   done
}
function sequential {
   for (( i=0; i<N; i++ ))
   do
   	erl -noshell -run sequential run $1 $2 $3 -run init stop
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
echo TotalCores
for (( cores=2; cores<=8; cores = cores*2 ))
do
    setCores $cores
    concurrent $Problem $Time 2
    sequential $Problem $Time 2
    hybrid $Problem $Time 2
done
## na koniec wlacz wszystkie rdzenie
setCores TotalCores