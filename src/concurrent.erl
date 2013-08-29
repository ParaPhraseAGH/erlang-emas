%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(concurrent).
-export([run/0, run/1, run/3, init/0]).

%% ====================================================================
%% API functions
%% ====================================================================

run(ProblemSize,Time,Islands) ->
  init(),
  {_Time,_} = timer:tc(fun spawner/3, [ProblemSize,Time,Islands]),
  cleanup(),
  ok.
  %io:format("Total time:   ~p s~n",[_Time/1000000]).

run([A,B,C]) ->
  run(list_to_integer(A),
    list_to_integer(B),
      list_to_integer(C)).

run() ->
  run(40,5000,2).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec spawner() -> float()
%% @doc Funkcja spawnujaca procesy nadzorujace dla kazdej wyspy
%% oraz czekajaca na koncowy wynik od nich.
spawner(ProblemSize,Time,Islands) ->
  Path = io_util:genPath("Concurrent",ProblemSize,Time,Islands),
  Supervisors = [spawn(conc_supervisor,run,[self(),X,Path,ProblemSize]) || X <- lists:seq(1,Islands)],
  respondToPorts(Supervisors,Islands),
  timer:sleep(Time),
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
