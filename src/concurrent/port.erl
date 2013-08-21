%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul implementujacy zachowanie portu, czyli areny do migracji.

-module(port).
-export([start/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(SupervisorPid,KingPid) -> ok | timeout
%% @doc Funkcja startujaca port. Na poczatku wysylane jest zapytanie do
%% krola o liste Aren, a pozniej nastepuje czekanie na odpowiedz.
start(Supervisor,King) ->
  random:seed(erlang:now()),
  Ref = erlang:monitor(process, King),
  King ! {self(),Ref,getArenas},
  receive
    {Ref,Arenas} ->
      erlang:demonitor(Ref, [flush]),
      receiver(Supervisor,Arenas);
    {'DOWN', Ref, process, King, Reason} ->
      io:format("The king is dead, long live the king!~n",[]),
      erlang:error(Reason)
  after 1000 ->
    io:format("Port ~p nie dostal wiadomosci z arenami~n",[self()]),
    timeout
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(SupervisorPid,Arenas) -> ok
%% @doc Funkcja glowna portu. Oczekuje na wiadomosci i albo umozliwia
%% migracje albo konczy prace portu.
receiver(Supervisor,Arenas) ->
  receive
    {Pid, Ref, emigration} ->
      Index = random:uniform(length(Arenas)),
      NewArenas = lists:nth(Index,Arenas),
      Pid ! {Ref,NewArenas},
      receiver(Supervisor,Arenas);
    {finish,Supervisor} ->
      emas_util:cleaner(Supervisor)
  end.
