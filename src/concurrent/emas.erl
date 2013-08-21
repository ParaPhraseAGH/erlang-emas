%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(emas).
-export([run/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run() -> ok
%% @doc Funkcja uruchamiajaca algorytm dla podanych w config.erl parametrow
run() ->
  init(),
  {Time,Result} = timer:tc(fun spawner/0, []),
  cleanup(),
  io:format("Total time:   ~p s~nFitness:     ~p~n",[Time/1000000,Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec spawner() -> float()
%% @doc Funkcja spawnujaca procesy nadzorujace dla kazdej wyspy
%% oraz czekajaca na koncowy wynik od nich.
spawner() ->
  Supervisors = [spawn(island,run,[self()]) || _ <- lists:seq(1,config:islandsNr())],
  Arenas = getArenas(Supervisors,[]),
  respondToPorts(Arenas,config:islandsNr()),
  receive
    {finalResult,From,Res} ->
      [X ! {close,self()} || X <- lists:delete(From,Supervisors)],
      Res
  end.

%% @spec getArenas(List1,List2) -> List3
%% @doc Funkcja odbiera wiadomosci od supervisorow i kompletuje liste
%% wszystkich aren w systemie. List1 to lista nadzorcow od ktorych mamy
%% dostac wiadomosc, List2 to akumulator gdzie gromadzimy krotki z arenami.
getArenas([],Arenas) -> Arenas;
getArenas(Supervisors,Arenas) ->
  receive
    {arenas,Pid,[Ring,Bar,Port]} ->
       getArenas(lists:delete(Pid,Supervisors),[{Ring,Bar,Port}|Arenas])
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

init() ->
  nothing.

cleanup() ->
  emas_util:clearInbox().
