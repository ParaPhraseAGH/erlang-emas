%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul implementujacy zachowanie ringu, czyli areny do walk.

-module(ring).
-export([start/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(SupervisorPid) -> ok
%% @doc Funkcja startujaca ring.
start(Supervisor) ->
  random:seed(erlang:now()),
  receiver(Supervisor,[]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(SupervisorPid,List1) -> ok
%% @doc Glowna funkcja ringu odpowiadajaca na wiadomosci. Ring moze otrzymac
%% zgloszenie do walki lub sygnal zamkniecia od supervisora.
%% List1 jest lista agentow oczekujacych na pojawienie sie wystarczajacej
%% liczby osobnikow w ringu do rozpoczecia walk.
receiver(Supervisor,Waitline) ->
  receive
    {Pid,Ref,{Fitness,Energy}} ->
      Agent = {Pid,Ref,Fitness,Energy},
      case length(Waitline) == config:fightNumber() - 1 of
        false -> receiver(Supervisor,[Agent|Waitline]);
        true ->
          NewAgents = doFight([Agent|Waitline]),
          emas_util:answer(NewAgents),
          receiver(Supervisor,[])
      end;
    {finish,Supervisor} ->
      emas_util:answer([{P,R,0,0} || {P,R,_,_} <- Waitline]), % wyslij wiadomosc konczaca rowniez do procesow w waitline
      emas_util:cleaner(Supervisor);
    _ ->
      io:format("Ring ~p dostal cos dziwnego~n",[self()])
  after config:arenaTimeout() ->
    case length(Waitline) of
      0 -> nothing;
      _ ->
        io:format("Ring ~p daje do walki niepelna liczbe osobnikow!~n",[self()]),
        Agents = doFight([Waitline]),
        emas_util:answer(lists:flatten(Agents)),
        receiver(Supervisor,[])
    end
  end.

%% @spec doFight(List1) -> List2
%% @doc Funkcja uruchamiajaca funkcje fightTwo/2 dla kazdej roznej
%% pary osobnikow w List1. List2 zawiera te wszystkie osobniki po walkach.
doFight([]) -> [];
doFight([H|T]) ->
  {NewH,NewT} = fightAll(H,T,[]),
  [NewH | doFight(NewT)].

%% @spec fightAll(A,ToFight,Fought) -> {A2,Rest}
%% @doc Funkcja uruchamiajaca funkcje fightTwo dla agenta A oraz
%% kazdego osobnika z listy ToFight. Agenci po walce przechowywani sa
%% w akumulatorze Fought i na koncu zwracani w krotce z agentem A po walkach.
fightAll(Agent,[],Fought) -> {Agent,Fought};
fightAll(Agent,[H|ToFight],Fought) ->
  {NewAgent,NewH}  = fightTwo(Agent,H),
  fightAll(NewAgent,ToFight,[NewH|Fought]).

%% @spec fightTwo(A1,A2) -> {A3,A4}
%% @doc Funkcja implementujaca walke dwoch osobnikow.
fightTwo({PidA,RefA,EvA, EnA}, {PidB,RefB,EvB, EnB}) ->
  AtoBtransfer =
    if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
      EvA >= EvB -> -erlang:min(config:fightTransfer(), EnB)
    end,
  {{PidA,RefA,EvA, EnA - AtoBtransfer}, {PidB,RefB,EvB, EnB + AtoBtransfer}}.