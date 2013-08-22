%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(concurrent).
-export([run/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run() -> ok
%% @doc Funkcja uruchamiajaca algorytm dla podanych w config.erl parametrow
run() ->
  Instance = "instancja",
  init(Instance),
  {Time,_} = timer:tc(fun spawner/1, [Instance]),
  cleanup(),
  io:format("Total time:   ~p s~n",[Time/1000000]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec spawner() -> float()
%% @doc Funkcja spawnujaca procesy nadzorujace dla kazdej wyspy
%% oraz czekajaca na koncowy wynik od nich.
spawner(Instance) ->
  Supervisors = [spawn(conc_island,run,[self(),X,Instance]) || X <- lists:seq(1,config:islandsNr())],
  Arenas = getArenas(length(Supervisors),[]),
  respondToPorts(Arenas,config:islandsNr()),
  timer:sleep(config:totalTime()),
  [Pid ! close || Pid <- Supervisors],
  finished.

%% @spec getArenas(List1,List2) -> List3
%% @doc Funkcja odbiera wiadomosci od supervisorow i kompletuje liste
%% wszystkich aren w systemie. List1 to lista nadzorcow od ktorych mamy
%% dostac wiadomosc, List2 to akumulator gdzie gromadzimy krotki z arenami.
getArenas(0,Arenas) -> Arenas;
getArenas(Supervisors,Arenas) ->
  receive
    {arenas,[Ring,Bar,Port]} ->
       getArenas(Supervisors - 1,[{Ring,Bar,Port}|Arenas])
  end.

%% @spec respondToPorts(List1,int()) -> ok
%% @doc Funkcja odpowiada wszystkim portom wysylajac im liste wszystkich aren.
%% Po poinformowaniu wszystkich portow (liczba podana w arg), funkcja zwraca ok.
respondToPorts(_,0) -> ok;
respondToPorts(Arenas,NoIslands) ->
  receive
    {Pid,Ref,getArenas} ->
      Pid ! {Ref,Arenas},
      respondToPorts(Arenas,NoIslands - 1)
  end.

init(Instance) ->
  file:make_dir(Instance).

cleanup() ->
  misc_util:clearInbox().
