%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami dotyczacymi operacji wejscia/wyjscia (wypisywanie na ekran, zapis do pliku).

-module(io_util).
-export([printSeq/1, printMoreStats/1, genPath/4, sumEnergy/1, printArenas/1]).

-include ("mas.hrl").

-type groups() :: [{agent_behaviour(),[agent()]}].

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Funkcja wypisujaca na ekran podstawowe parametry dla modelu sekwencyjnego. W argumencie przesylana jest lista wysp.
-spec printSeq([island()]) -> ok.
printSeq([]) -> ok;
printSeq([Island|T]) ->
    io:format("Island ~p Fitness ~p Population ~p Energy ~p~n",[length(T),misc_util:result(Island),length(Island),sumEnergy(Island)]),
    printSeq(T).

%% @doc Funkcja oblicza sume energii w danej populacji.
-spec sumEnergy([agent()]) -> integer().
sumEnergy(Agents) ->
    lists:foldl(fun({_,_,E},Acc) -> Acc + E end,0,Agents).

%% @doc Funkcja wypisuje dodatkowe informacje na podstawie przeslanej
%% struktury populacji.
-spec printMoreStats(groups()) -> any().
printMoreStats(Groups) ->
    D = lists:flatten([X || {death,X} <- Groups]),
    F = lists:flatten([X || {fight,X} <- Groups]),
    R = lists:flatten([X || {reproduction,X} <- Groups]),
    M = lists:flatten([X || {migration,X} <- Groups]),
    io:format("Dying: ~p    Fighting: ~p    Reproducing: ~p    Leaving: ~p~n",[length(D),length(F),length(R),length(M)]),
    io:format("Population: ~p, Energy: ~p~n",[length(D ++ M ++ F ++ R),sumEnergy(M ++ F ++ R)]).

-spec printArenas([{atom(),pid()}]) -> ok.
printArenas([]) ->
    io:format("#supervisor: ~p~n~n",[self()]);
printArenas([{Name,Pid}|Arenas]) ->
    io:format("#~p: ~p~n",[Name,Pid]),
    printArenas(Arenas).

%% @doc Funkcja generujaca sciezke dostepu wraz z nazwa folderu dla danego uruchomienia algorytmu. Wyznaczana jest sciezka
%% dostepu w zaleznosci od parametrow algorytmu i tworzony jest kolejny folder dla kazdego uruchomienia (instance).
%% !!!W obecnej wersji algorytmu nieuzywana!!!
-spec genPath(string(),pos_integer(),pos_integer(),pos_integer()) -> string().
genPath(AlgType,Problem,TTime,Islands) ->
    catch file:make_dir(AlgType),
    Time = TTime div 1000,
    Param = integer_to_list(Problem) ++ "_" ++ integer_to_list(Time) ++ "_" ++ integer_to_list(Islands),
    Path = filename:join([AlgType, Param]),
    catch file:make_dir(Path),
    {ok,Instances} = file:list_dir(Path),
    Path2 =  filename:join([Path,assignName(Instances,0)]),
    file:make_dir(Path2),
    Path2.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc Funkcja wyznacza nazwe dla nowego folderu na podstawie otrzymanej listy juz istniejacych katalogow.
%% !!!W obecnej wersji algorytmu nieuzywana!!!
-spec assignName([string()],integer()) -> string().
assignName(Files,N) ->
    Name = "instance" ++ integer_to_list(N),
    case lists:member(Name,Files) of
        true -> assignName(Files,N+1);
        false -> Name
    end.