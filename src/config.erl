%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Modul zawierajacy parametry algorytmu.

-module(config).
-compile(export_all).

%% ====================================================================
%% PARAMAS PARAMETERS
%% ====================================================================

%% @doc Nazwa modulu implementujacego behaviour agent_env
agent_env() -> emas.

%% @doc Topologia wysp
topology() -> mesh.

%% @doc Domyslna sciezka zapisu logow (folder musi istniec). Atom standard_io powoduje wypisanie na standardowe wyjscie
logDir() -> standard_io.

%% @doc Liczba wysp
islands() -> 4.
%% @doc Poczatkowa wielkosc populacji
populationSize() -> 100.

%% @doc Co ile milisekund wpisywany jest wynik do pliku
writeInterval() -> 1000.
%% @doc Liczba milisekund bezczynnosci po ktorych arena powinna wyrzucic blad | infinity
arenaTimeout() -> 5000.