%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0

-module(arenas).
-export([startBar/1, startRing/1, startPort/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(SupervisorPid) -> ok
%% @doc Funkcja startujaca ring.
startRing(Supervisor) ->
  random:seed(erlang:now()),
  ring(Supervisor,[]).

%% @spec start(SupervisorPid,KingPid) -> ok | timeout
%% @doc Funkcja startujaca port. Na poczatku wysylane jest zapytanie do
%% krola o liste Aren, a pozniej nastepuje czekanie na odpowiedz.
startPort(Supervisor,King) ->
  random:seed(erlang:now()),
  Ref = erlang:monitor(process, King),
  King ! {self(),Ref,getArenas},
  receive
    {Ref,Arenas} ->
      erlang:demonitor(Ref, [flush]),
      port(Supervisor,Arenas);
    {'DOWN', Ref, process, King, Reason} ->
      io:format("The king is dead, long live the king!~n",[]),
      erlang:error(Reason)
  after 1000 ->
    io:format("Port ~p nie dostal wiadomosci z arenami~n",[self()]),
    timeout
  end.

%% @spec start(SupervisorPid,RingPid,PortPid) -> ok
%% @doc Funkcja startujaca bar.
startBar(Supervisor) ->
  random:seed(erlang:now()),
  bar(Supervisor,[]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(SupervisorPid,List1,RingPid,PortPid) -> ok
%% @doc Glowna funkcja baru odpowiadajaca na wiadomosci. Bar moze otrzymac
%% zgloszenie do krzyzowania/mutacji lub sygnal zamkniecia od supervisora.
%% List1 jest lista agentow oczekujacych na pojawienie sie wystarczajacej
%% liczby osobnikow w barze do rozpoczecia reprodukcji.
bar(Supervisor,Waitline) ->
  receive
    {Pid1,Ref1,Agent1} -> %{Solution,Fitness,Energy} = Agent,
      case Waitline of
        [] -> bar(Supervisor,[{Pid1,Ref1,Agent1}]);
        [{Pid2,Ref2,Agent2}] ->
          [{_,_,NewEnergy1},{_,_,NewEnergy2},NewAgent1,NewAgent2] = evolution:doReproduce({Agent1,Agent2}),
          Pid1 ! {Ref1,NewEnergy1},
          Pid2 ! {Ref2,NewEnergy2},
          Supervisor ! {newAgents,[NewAgent1,NewAgent2]},
          bar(Supervisor,[])
      end;
    {finish,Supervisor} ->
      answer([{Pid,Ref,0,0} || {Pid,Ref,_} <- Waitline]),
      cleaner(Supervisor);
    _ ->
      io:format("Bar ~p dostal cos dziwnego~n",[self()])
  after config:arenaTimeout() ->
    case Waitline of
      [] -> nothing;
      [{Pid,Ref,Agent}] ->
        io:format("Bar ~p reprodukuje pojedynczego osobnika!~n",[self()]),
        [{_,_,NewEnergy},NewAgent] = evolution:doReproduce({Agent}),
        Pid ! {Ref,NewEnergy},
        Supervisor ! {newAgents,[NewAgent]},
        bar(Supervisor,[])
    end
  end.

%% @spec receiver(SupervisorPid,List1) -> ok
%% @doc Glowna funkcja ringu odpowiadajaca na wiadomosci. Ring moze otrzymac
%% zgloszenie do walki lub sygnal zamkniecia od supervisora.
%% List1 jest lista agentow oczekujacych na pojawienie sie wystarczajacej
%% liczby osobnikow w ringu do rozpoczecia walk.
ring(Supervisor,Waitline) ->
  receive
    {Pid,Ref,{Fitness,Energy}} ->
      Agent = {{Pid,Ref},Fitness,Energy},
      case length(Waitline) == config:fightNumber() - 1 of
        false -> ring(Supervisor,[Agent|Waitline]);
        true ->
          NewAgents = evolution:eachFightsAll([Agent|Waitline]),
          answer([{P,R,F,E} || {{P,R},F,E} <- NewAgents]),
          ring(Supervisor,[])
      end;
    {finish,Supervisor} ->
      answer([{P,R,0,0} || {{P,R},_,_} <- Waitline]), % wyslij wiadomosc konczaca rowniez do procesow w waitline
      cleaner(Supervisor);
    _ ->
      io:format("Ring ~p dostal cos dziwnego~n",[self()])
  after config:arenaTimeout() ->
    case length(Waitline) of
      0 -> nothing;
      _ ->
        io:format("Ring ~p daje do walki niepelna liczbe osobnikow!~n",[self()]),
        Agents = evolution:eachFightsAll([Waitline]),
        answer(lists:flatten(Agents)),
        ring(Supervisor,[])
    end
  end.

%% @spec receiver(SupervisorPid,Arenas) -> ok
%% @doc Funkcja glowna portu. Oczekuje na wiadomosci i albo umozliwia
%% migracje albo konczy prace portu.
port(Supervisor,Arenas) ->
  receive
    {Pid, Ref, emigration} ->
      Index = random:uniform(length(Arenas)),
      NewArenas = lists:nth(Index,Arenas),
      Pid ! {Ref,NewArenas},
      port(Supervisor,Arenas);
    {finish,Supervisor} ->
      cleaner(Supervisor)
  end.

%% @spec answer(AgentList) -> ok
%% @doc Funkcja wysyla wiadomosci do wszystkich agentow w agent list
%% o ich energii.
answer([]) -> ok;
answer([{Pid,Ref,_,Energy}|Tail]) ->
  Pid ! {Ref,Energy},
  answer(Tail).

%% @spec cleaner(SupervisorPid) -> ok
%% @doc Funkcja uruchamiana pod koniec zycia przez areny, odsylajaca na
%% wszystkie zgloszenia sygnal zabijajacy. Dzieki temu gina wszyscy agenci
%% w systemie. Po zakonczeniu funkcji zakonczony jest proces
cleaner(Supervisor) ->
  receive
    {Pid,_Ref,emigration} ->
      exit(Pid,finished),
      cleaner(Supervisor);
    {Pid,Ref,_} ->
      Pid ! {Ref,0},
      cleaner(Supervisor)
  after config:arenaTimeout() ->
    Supervisor ! {finished,self()},
    exit(normal)
  end.