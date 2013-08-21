%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul implementujacy zachowanie baru, czyli areny do reprodukcji.

-module(bar).
-export([start/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(SupervisorPid,RingPid,PortPid) -> ok
%% @doc Funkcja startujaca bar.
start(Supervisor,Ring,Port) ->
  random:seed(erlang:now()),
  receiver(Supervisor,[],Ring,Port).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(SupervisorPid,List1,RingPid,PortPid) -> ok
%% @doc Glowna funkcja baru odpowiadajaca na wiadomosci. Bar moze otrzymac
%% zgloszenie do krzyzowania/mutacji lub sygnal zamkniecia od supervisora.
%% List1 jest lista agentow oczekujacych na pojawienie sie wystarczajacej
%% liczby osobnikow w barze do rozpoczecia reprodukcji.
receiver(Supervisor,Waitline,Ring,Port) ->
  receive
    {Pid1,Ref1,Agent1} -> %{Solution,Fitness,Energy} = Agent,
      case Waitline of
        [] -> receiver(Supervisor,[{Pid1,Ref1,Agent1}],Ring,Port);
        [{Pid2,Ref2,Agent2}] ->
          {{_,_,NewEnergy1},{_,_,NewEnergy2},NewAgent1,NewAgent2} = doReproduce(Agent1,Agent2),
          report(Supervisor,Agent1,Agent2,NewAgent1,NewAgent2),
          Pid1 ! {Ref1,NewEnergy1},
          Pid2 ! {Ref2,NewEnergy2},
          spawn(agent,start,[Ring,self(),Port,NewAgent1]),
          spawn(agent,start,[Ring,self(),Port,NewAgent2]),
          receiver(Supervisor,[],Ring,Port)
      end;
    {finish,Supervisor} ->
      emas_util:answer([{Pid,Ref,0,0} || {Pid,Ref,_} <- Waitline]),
      emas_util:cleaner(Supervisor);
    _ ->
      io:format("Bar ~p dostal cos dziwnego~n",[self()])
  after config:arenaTimeout() ->
    case Waitline of
      [] -> nothing;
      [{Pid,Ref,Agent}] ->
        io:format("Bar ~p reprodukuje pojedynczego osobnika!~n",[self()]),
        {{_,_,NewEnergy},NewAgent} = doReproduce(Agent),
        Pid ! {Ref,NewEnergy},
        spawn(agent,start,[Ring,self(),Port,NewAgent]),
        receiver(Supervisor,[],Ring,Port)
    end
  end.

%% @spec doReproduce(A1) -> {A2,A3}
%% @doc Funkcja reprodukujaca agenta. Z jednego osobnika powstaja dwa nowe.
doReproduce({SolA, EvA, EnA}) ->
  SolB = genetic:reproduction(SolA),
  EvB = genetic:evaluation(SolB),
  AtoBtransfer = erlang:min(config:reproductionTransfer(), EnA),
  {{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}}.

%% @spec doReproduce(Agent1,Agent2) -> {A3,A4,A5,A6}
%% @doc Funkcja reprodukujaca agentow. Z dwoch osobnikow powstaja cztery nowe.
doReproduce({SolA, EvA, EnA}, {SolB, EvB, EnB}) ->
  [SolC, SolD] = genetic:reproduction(SolA, SolB),
  [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
  [AtoCTransfer, BtoDTransfer] = [ erlang:min(config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
  {{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}}.

%% @spec report(Pid,A1,A2,A3,A4) -> ok
%% @doc Funkcja wysylajaca pod podany Pid najlepszy fitness z podanych agentow.
report(Pid,{_,F1,_},{_,F2,_},{_,F3,_},{_,F4,_}) ->
  Pid ! {result,lists:max([F1,F2,F3,F4])}.