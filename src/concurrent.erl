%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(concurrent).
-export([run/0, run/1, run/4, getAddresses/1]).

%% ====================================================================
%% API functions
%% ====================================================================

run(ProblemSize,Time,Islands,Path) ->
  init(),
  {_Time,_} = timer:tc(fun spawner/4, [ProblemSize,Time,Islands,Path]),
  cleanup(),
  ok.
  %io:format("Total time:   ~p s~n",[_Time/1000000]).

run([A,B,C,D]) ->
  run(list_to_integer(A),
    list_to_integer(B),
      list_to_integer(C),D).

run() ->
  file:make_dir("tmp"),
  run(40,60000,2,"tmp").

getAddresses(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(),Ref,getAdresses},
  receive
    {Ref,AllSupervisors} ->
      erlang:demonitor(Ref, [flush]),
      AllSupervisors;
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("The king is dead, long live the king!~n",[]),
      erlang:error(Reason)
  after 1000 ->
    io:format("Port ~p nie dostal wiadomosci z adresami~n",[self()]),
    erlang:error(timeout)
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec spawner() -> float()
%% @doc Funkcja spawnujaca procesy nadzorujace dla kazdej wyspy
%% oraz czekajaca na koncowy wynik od nich.
spawner(ProblemSize,Time,Islands,Path) ->
  %Path = io_util:genPath("Concurrent",ProblemSize,Time,Islands),
  Supervisors = [spawn(conc_supervisor,run,[self(),X,Path,ProblemSize]) || X <- lists:seq(1,Islands)],
  giveAddresses(Supervisors,Islands),
  timer:sleep(Time),
  [conc_supervisor:close(Pid) || Pid <- Supervisors],
  finished.

%% @spec giveAdresses(List1,int()) -> ok
%% @doc Funkcja odpowiada wszystkim portom wysylajac im liste wszystkich aren.
%% Po poinformowaniu wszystkich portow (liczba podana w arg), funkcja zwraca ok.
giveAddresses(_,0) -> ok;
giveAddresses(Supervisors,NoIslands) ->
  receive
    {Pid,Ref,getAdresses} ->
      Pid ! {Ref,Supervisors},
      giveAddresses(Supervisors,NoIslands - 1)
  after config:supervisorTimeout() ->
    erlang:error(noMsgFromPorts),
    timeout
  end.

init() ->
  nothing.

cleanup() ->
  misc_util:clearInbox().
