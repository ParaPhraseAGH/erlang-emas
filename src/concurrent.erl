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
  init(),
  {Time,_} = timer:tc(fun spawner/0, []),
  cleanup(),
  io:format("Total time:   ~p s~n",[Time/1000000]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec spawner() -> float()
%% @doc Funkcja spawnujaca procesy nadzorujace dla kazdej wyspy
%% oraz czekajaca na koncowy wynik od nich.
spawner() ->
  Path = io_util:genPath("Concurrent"),
  Supervisors = [spawn(conc_supervisor,run,[self(),X,Path]) || X <- lists:seq(1,config:islandsNr())],
  respondToPorts(Supervisors,config:islandsNr()),
  timer:sleep(config:totalTime()),
  [Pid ! close || Pid <- Supervisors],
  finished.

%% @spec respondToPorts(List1,int()) -> ok
%% @doc Funkcja odpowiada wszystkim portom wysylajac im liste wszystkich aren.
%% Po poinformowaniu wszystkich portow (liczba podana w arg), funkcja zwraca ok.
respondToPorts(_,0) -> ok;
respondToPorts(Supervisors,NoIslands) ->
  receive
    {Pid,Ref,getAdresses} ->
      Pid ! {Ref,Supervisors},
      respondToPorts(Supervisors,NoIslands - 1)
  after config:supervisorTimeout() ->
    erlang:error(noMsgFromPorts),
    timeout
  end.

init() ->
  nothing.

cleanup() ->
  misc_util:clearInbox().
