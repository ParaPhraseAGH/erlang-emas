%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0

-module(arenas).
-export([startBar/1, startRing/1, startPort/2, call/2, close/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(SupervisorPid) -> ok
%% @doc Funkcja startujaca ring.
startRing(_Supervisor) ->
  random:seed(erlang:now()),
  ring([]).

%% @spec start(SupervisorPid,KingPid) -> ok | timeout
%% @doc Funkcja startujaca port. Na poczatku wysylane jest zapytanie do
%% krola o liste supervisorow, a pozniej nastepuje czekanie na odpowiedz.
startPort(Supervisor,King) ->
  random:seed(erlang:now()),
  AllSupervisors = concurrent:getAddresses(King),
  port(Supervisor,AllSupervisors).

%% @spec start(SupervisorPid) -> ok
%% @doc Funkcja startujaca bar.
startBar(Supervisor) ->
  random:seed(erlang:now()),
  bar(Supervisor,[]).

%% @spec call(Message,ArenaPid) -> Answer
%% @doc Funkcja wysyla podana wiadomosc do danej areny i zwraca otrzymana
%% odpowiedz.
call(Msg,ArenaPid) ->
  Ref = erlang:monitor(process, ArenaPid),
  ArenaPid ! {self(), Ref, Msg},
  receive
    {Ref, Ans} ->
      erlang:demonitor(Ref, [flush]),
      Ans;
    {'DOWN', Ref, process, ArenaPid, Reason} ->
      io:format("Arena do ktorej chce pisac proces ~p nie istnieje!~n",[self()]),
      erlang:error(Reason)
  after config:processTimeout() -> % docelowo nie bedzie timeoutu
    io:format("Proces ~p nie doczekal sie odpowiedzi od areny ~p!~n",[self(),ArenaPid]),
    exit(timeout)
  end.

close(Pid) ->
  Pid ! {finish,self()}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(SupervisorPid,List1,RingPid,PortPid) -> ok
%% @doc Glowna funkcja baru odpowiadajaca na wiadomosci. Bar moze otrzymac
%% zgloszenie do krzyzowania/mutacji lub sygnal zamkniecia od supervisora.
%% List1 jest lista agentow oczekujacych na pojawienie sie wystarczajacej
%% liczby osobnikow w barze do rozpoczecia reprodukcji.
bar(Supervisor,Waitlist) ->
  receive
    {Pid1,Ref1,Agent1} -> %{Solution,Fitness,Energy} = Agent,
      case Waitlist of
        [] -> bar(Supervisor,[{Pid1,Ref1,Agent1}]);
        [{Pid2,Ref2,Agent2}] ->
          [{_,_,NewEnergy1},{_,_,NewEnergy2},NewAgent1,NewAgent2] = evolution:doReproduce({Agent1,Agent2}),
          Pid1 ! {Ref1,NewEnergy1},
          Pid2 ! {Ref2,NewEnergy2},
          conc_supervisor:sendAgents(Supervisor,[NewAgent1,NewAgent2]),
          bar(Supervisor,[])
      end;
    {finish,_Supervisor} ->
      answer([{{Pid,Ref},0,0} || {Pid,Ref,_} <- Waitlist]),
      cleaner();
    _ ->
      io:format("Bar ~p dostal cos dziwnego~n",[self()])
  after config:arenaTimeout() ->
    case Waitlist of
      [] ->
        bar(Supervisor,[]);
      [{Pid,Ref,Agent}] ->
        io:format("Bar ~p reprodukuje pojedynczego osobnika!~n",[self()]),
        [{_,_,NewEnergy},NewAgent] = evolution:doReproduce({Agent}),
        Pid ! {Ref,NewEnergy},
        conc_supervisor:sendAgents(Supervisor,[NewAgent]),
        bar(Supervisor,[])
    end
  end.

%% @spec receiver(SupervisorPid,List1) -> ok
%% @doc Glowna funkcja ringu odpowiadajaca na wiadomosci. Ring moze otrzymac
%% zgloszenie do walki lub sygnal zamkniecia od supervisora.
%% List1 jest lista agentow oczekujacych na pojawienie sie wystarczajacej
%% liczby osobnikow w ringu do rozpoczecia walk.
ring(Waitlist) ->
  receive
    {Pid,Ref,{_Solution,Fitness,Energy}} ->
      Agent = {{Pid,Ref},Fitness,Energy},
      case length(Waitlist) == config:fightNumber() - 1 of
        false -> ring([Agent|Waitlist]);
        true ->
          NewAgents = evolution:eachFightsAll([Agent|Waitlist]),
          answer(NewAgents), % moze potrzebne flatten
          ring([])
      end;
    {finish,_Supervisor} ->
      answer([{{P,R},0,0} || {{P,R},_,_} <- Waitlist]), % wyslij wiadomosc konczaca rowniez do procesow w waitline
      cleaner();
    _ ->
      io:format("Ring ~p dostal cos dziwnego~n",[self()])
  after config:arenaTimeout() ->
    case length(Waitlist) of
      0 ->
        ring([]);
      _ ->
        io:format("Ring ~p daje do walki niepelna liczbe osobnikow!~n",[self()]),
        Agents = evolution:eachFightsAll(Waitlist),
        answer(Agents),  % moze niepotrzebne flatten
        ring([])
    end
  end.

%% @spec receiver(SupervisorPid,OtherSupervisors) -> ok
%% @doc Funkcja glowna portu. Oczekuje na wiadomosci i albo umozliwia
%% migracje albo konczy prace portu.
port(Supervisor,AllSupervisors) ->
  receive
    {Pid, HisRef, emigration} ->
      Index = random:uniform(length(AllSupervisors)),
      NewSupervisor = lists:nth(Index,AllSupervisors),
      case conc_supervisor:unlinkAgent(Supervisor,Pid) of
        ok -> ok;
        _ -> port(Supervisor,AllSupervisors)
      end,
      case conc_supervisor:linkAgent(NewSupervisor,Pid,HisRef) of
        ok -> ok;
        _ ->  exit(Pid,finished)
      end,
      port(Supervisor,AllSupervisors);
    {finish,Supervisor} ->
      cleaner()
  end.


%% @spec answer(AgentList) -> ok
%% @doc Funkcja wysyla wiadomosci do wszystkich agentow w agent list
%% o ich energii.
answer([]) -> ok;
answer([{{Pid,Ref},_,Energy}|Tail]) ->
  Pid ! {Ref,Energy},
  answer(Tail).

%% @spec cleaner() -> ok
%% @doc Funkcja uruchamiana pod koniec zycia przez areny, odsylajaca na
%% wszystkie zgloszenia sygnal zabijajacy. Dzieki temu gina wszyscy agenci
%% w systemie. Po zakonczeniu funkcji zakonczony jest proces
cleaner() ->
  receive
    {Pid,_Ref,emigration} ->
      exit(Pid,finished),
      cleaner();
    {Pid,Ref,_} ->
      Pid ! {Ref,0},
      cleaner()
  after config:arenaTimeout() ->
    exit(normal)
  end.