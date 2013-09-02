%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0

-module(evolution).
-export([sendToWork/1, doReproduce/1, doFight/1, doMigrate/1, eachFightsAll/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec sendToWork({atom(),List1}) -> List2
%% @doc Funkcja dostaje atom precyzujacy klase agentow i ich liste,
%% a nastepnie wykonuje odpowiednie operacje dla kazdej z klas.
%% Funkcja zwraca liste agentow po przetworzeniu.
sendToWork({death, _}) ->
  [];
sendToWork({fight, Agents}) ->
  lists:flatmap(fun doFight/1, optionalPairs(Agents,[]));
sendToWork({reproduction,Agents}) ->
  lists:flatmap(fun doReproduce/1, optionalPairs(Agents,[]));
sendToWork({migration,Agents}) ->
  case Agents of
    [] -> [];
    [H|T] ->
      whereis(supervisor) ! {agent,self(),H},
      sendToWork({migration,T})
  end.

%% @spec eachFightsAll(List1) -> List2
%% @doc Funkcja uruchamiajaca funkcje fightTwo/2 dla kazdej roznej
%% pary osobnikow w List1. List2 zawiera te wszystkie osobniki po walkach.
eachFightsAll([]) -> [];
eachFightsAll([H|T]) ->
  {NewH,NewT} = oneFightsRest(H,T,[]),
  [NewH | eachFightsAll(NewT)].

%% @spec doFight({Agent1}) -> [Agent2]
%% @doc Funkcja implementujaca logike "walki" pojedynczego agenta.
%% Zwracany jest ten sam agent w liscie.
doFight({A}) -> [A];
%% @spec doFight({Agent1,Agent2}) -> [Agent3,Agent4]
%% @doc Funkcja implementujaca logike walki dwoch agentow.
%% Zwracana jest lista dwoch przetworzonych agentow.
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  AtoBtransfer =
    if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
      EvA >= EvB -> -erlang:min(config:fightTransfer(), EnB)
    end,
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].

%% @spec doReproduce({Agent1}) -> List
%% @doc Funkcja implementujaca logike reprodukcji pojedynczego agenta.
%% Zwracana jest dwojka agentow w liscie.
doReproduce({{SolA, EvA, EnA}}) ->
  SolB = genetic:reproduction(SolA),
  EvB = genetic:evaluation(SolB),
  AtoBtransfer = erlang:min(config:reproductionTransfer(), EnA),
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
%% @spec doReproduce({Agent1,Agent2}) -> [Agent3,Agent4,Agent5,Agent6]
%% @doc Funkcja implementujaca logike reprodukcji dwoch agentow.
%% Zwracanych jest czterech agentow w liscie.
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  [SolC, SolD] = genetic:reproduction(SolA, SolB),
  [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
  [AtoCTransfer, BtoDTransfer] = [ erlang:min(config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
  [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].

%% @spec doMigrate(List1) -> List2
%% @doc Funkcja dokonujaca migracji. Wyznaczana jest liczba agentow,
%% ktorzy powinni ulec przesiedleniu, dokonywana migracja i zwracana
%% przetworzona lista wysp.
doMigrate(Islands) ->
  {Gathered,NewIslands} = gather(Islands,[],[]),
  Shuffled = misc_util:shuffle(Gathered),
  append(Shuffled,NewIslands,[],length(Shuffled) div length(NewIslands)).

%% ====================================================================
%% Internal functions
%% ====================================================================

append(Immigrants,[Island],Acc,_) ->
  if Immigrants == [] ->
    NewIsland = Island;
  Immigrants /= [] ->
    NewIsland = [Immigrants|Island]
  end,
  [lists:flatten(NewIsland)|Acc];
append(Immigrants,[Island|T],Acc,N) ->
  {A,NewImmigrants} = lists:split(N,Immigrants),
  if A == [] ->
    NewIslands = [Island|Acc];
  A /= [] ->
    NewIslands = [lists:flatten([A|Island])|Acc]
  end,
  append(NewImmigrants,T,NewIslands,N).

gather([],Islands,Emigrants) ->
  {Emigrants,Islands};
gather([I|T],Acc,Emigrants) ->
  N = config:migrationProbability() * length(I),
  if N == 0 ->
    gather(T,[I|Acc],Emigrants);
  N < 1 ->
    case random:uniform() < N of
      true ->
        {NewEmigrants,NewIsland} = lists:split(1,I),
        gather(T,[NewIsland|Acc],[NewEmigrants|Emigrants]);
      false ->
        gather(T,[I|Acc],Emigrants)
    end;
  N >= 1 ->
    {NewEmigrants,NewIsland} = lists:split(trunc(N),I),
    gather(T,[NewIsland|Acc],[NewEmigrants|Emigrants])
  end.


%% @spec oneFightsRest(A,ToFight,Fought) -> [A2,Rest]
%% @doc Funkcja uruchamiajaca funkcje fightTwo dla agenta A oraz
%% kazdego osobnika z listy ToFight. Agenci po walce przechowywani sa
%% w akumulatorze Fought i na koncu zwracani w krotce z agentem A po walkach.
oneFightsRest(Agent,[],Fought) -> {Agent,Fought};
oneFightsRest(Agent,[H|ToFight],Fought) ->
  [NewAgent,NewH]  = doFight({Agent,H}),
  oneFightsRest(NewAgent,ToFight,[NewH|Fought]).

%% @spec optionalPairs(List1,List2) -> List3
%% @doc Funkcja dzielaca podana liste agentow na pary. Tail recursion.
optionalPairs([],Acc) -> Acc;
optionalPairs([A],Acc) -> [{A}|Acc];
optionalPairs([A,B|L],Acc) -> optionalPairs(L,[{A,B}|Acc]).