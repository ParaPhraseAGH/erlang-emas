%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Modul zawierajacy parametry algorytmu.

-module(config).
-compile(export_all).

%% ====================================================================
%% PARAMAS PARAMETERS
%% ====================================================================

%% @doc Topologia wysp
topology() -> mesh.
%% @doc Domyslna sciezka zapisu logow (folder musi istniec). Atom standard_io powoduje wypisanie na standardowe wyjscie
logDir() -> standard_io.
%% @doc Liczba wysp
islands() -> 2.
%% @doc Poczatkowa wielkosc populacji
populationSize() -> 100.
%% @doc Prawdopodobienstwo migracji dowolnego osobnika o niezerowej energii na inna wyspe
migrationProbability() -> 0.0001.

%% @doc Co ile milisekund wpisywany jest wynik do pliku
writeInterval() -> 1000.
%% @doc Ilosc milisekund, ktore czeka supervisor wyspy na jakas wiadomosc. Jak czas minie zamyka wyspe.
supervisorTimeout() -> 5000.
%% @doc Ilosc milisekund, ktore czeka topology na jakas wiadomosc
topologyTimeout() -> 10000.
%% @doc Ilosc milisekund, ktore czeka arena na jakas wiadomosc
arenaTimeout() -> 3000.

%% @doc Nazwa modulu implementujacego behaviour agent_env
agent_env() -> emas.