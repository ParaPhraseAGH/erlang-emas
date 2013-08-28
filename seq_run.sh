#/bin/bash
cd bin
N=10 # ilosc wykonanych testow dla kazdej konfiguracji
function run {
   for (( i=0; i<N; i++ ))
   do
   	erl -noshell -run sequential run $1 $2 $3 -run init stop
   done
}
run 10 1000 5