%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(concurrent).
-export([start/0, start/1, start/5, getAddresses/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start(ProblemSize,Time,Islands,Topology,Path) ->
  %Path = io_util:genPath("Concurrent",ProblemSize,Time,Islands),
  misc_util:clearInbox(),
  topology:start_link(Islands,Topology),
  Supervisors = [conc_supervisor:start(self(),X,Path,ProblemSize) || X <- lists:seq(1,Islands)],
  giveAddresses(Supervisors,Islands),
  timer:sleep(Time),
  [conc_supervisor:close(Pid) || Pid <- Supervisors],
  topology:close().

start([A,B,C,D,E]) ->
  start(list_to_integer(A),
    list_to_integer(B),
      list_to_integer(C),
        list_to_atom(D),E).

start() ->
  file:make_dir("tmp"),
  start(40,5000,2,mesh,"tmp").

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