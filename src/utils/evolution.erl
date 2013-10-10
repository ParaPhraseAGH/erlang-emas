%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami dotyczacymi ewolucji czyli przechodzenia jednej generacji w kolejna.
%% Sa tu rowniez funkcje implementujace migracje miedzy wyspami
-module(evolution).
-export([sendToWork/1, doReproduce/1, doFight/1, doMigrate/1, eachFightsAll/1]).

-type task() :: death | fight | reproduction | migration.
-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type island() :: [agent()].
-type fighter() :: {term(), Fitness::float(), Energy::pos_integer()}. % agent niekoniecznie zawierajacy solution. Czasem jest zamiast tego podstawiany {Pid,Ref}, aby moc pozniej odeslac energie.

%% ====================================================================
%% API functions
%% ====================================================================
-spec sendToWork({task(),[agent()]}) -> [agent()].
%% @doc Funkcja dostaje atom precyzujacy klase agentow i ich liste,
%% a nastepnie wykonuje odpowiednie operacje dla kazdej z klas.
%% Funkcja zwraca liste agentow po przetworzeniu.
sendToWork({death, _}) ->
  [];
sendToWork({fight, Agents}) ->
  lists:flatmap(fun doFight/1, optionalPairs(Agents,[]));
sendToWork({reproduction,Agents}) ->
  lists:flatmap(fun doReproduce/1, optionalPairs(Agents,[]));
sendToWork({migration,[]}) ->
  [];
sendToWork({migration,[Agent|T]}) when tuple_size(Agent) == 3 ->
  hybrid:sendAgent(Agent),
  sendToWork({migration,T});
sendToWork({migration,[{From,Agent}|T]}) ->
  [{topology:getDestination(From),Agent} | sendToWork({migration,T})].

-spec eachFightsAll([fighter()]) -> [fighter()].
%% @doc Funkcja implementujaca walke "kazdy z kazdym" dla listy agentow w argumencie.
%% Zwracana jest lista agentow po walkach.
eachFightsAll([]) -> [];
eachFightsAll([H|T]) ->
  {NewH,NewT} = oneFightsRest(H,T,[]),
  [NewH | eachFightsAll(NewT)].

-spec doFight({fighter()} | {fighter(),fighter()}) -> [fighter()].
%% @doc Funkcja implementujaca logike "walki" pojedynczego agenta.
%% Zwracany jest ten sam agent w liscie.
doFight({A}) -> [A];
%% @doc Funkcja implementujaca logike walki dwoch agentow.
%% Zwracana jest lista dwoch przetworzonych agentow.
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  AtoBtransfer =
    if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
      EvA >= EvB -> -erlang:min(config:fightTransfer(), EnB)
    end,
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].

-spec doReproduce({agent()} | {agent(),agent()}) -> [agent()].
%% @doc Funkcja implementujaca logike reprodukcji pojedynczego agenta.
%% Zwracanych jest dwoje agentow w liscie.
doReproduce({{SolA, EvA, EnA}}) ->
  SolB = genetic:reproduction(SolA),
  EvB = genetic:evaluation(SolB),
  AtoBtransfer = erlang:min(config:reproductionTransfer(), EnA),
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
%% @doc Funkcja implementujaca logike reprodukcji dwoch agentow.
%% Zwracanych jest czterech agentow w liscie.
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  [SolC, SolD] = genetic:reproduction(SolA, SolB),
  [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
  [AtoCTransfer, BtoDTransfer] = [ erlang:min(config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
  [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].

-spec doMigrate([island()]) -> {non_neg_integer(),[island()]}.
%% @doc Funkcja dokonujaca migracji dla algorytmu sekwencyjnego. Najpierw z kazdej wyspy pobierana jest statystyczna
%% liczba agentow, ktorzy powinni ulec migracji. Dla kazdej grupy emigrantow wyznaczana jest wyspa docelowa
%% i sa oni do niej dopisywani. Zwracana jest lista wysp po dokonanej mirgacji.
doMigrate(Islands)->
  {Gathered,NewIslands} = gather(Islands,[],[]),
  {length(Gathered),append(Gathered,lists:reverse(NewIslands))}.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec append([{[agent()],integer()}],[island()]) -> [island()].
%% @doc Funkcja dla kazdej grupy emigrantow z listy wyznacza wyspe docelowa oraz dokleja ich do tamtejszej populacji.
append([],Islands) -> Islands;
append([{Immigrants,From}|T],Islands) ->
  Destination = topology:getDestination(From),
  NewIslands = misc_util:mapIndex(Immigrants,Destination,Islands,fun lists:append/2),
  append(T,NewIslands).

-spec gather([island()],[island()],[{[agent()],integer()}]) -> {[{[agent()],integer()}],[island()]}.
%% @doc Funkcja wyznacza ile srednio agentow z danej populacji powinno emigrowac i przesuwa ich do specjalnej listy emigrantow.
%% Zwracana jest wyznaczona lista emigrantow oraz uszczuplona lista wysp.
gather([],Islands,Emigrants) ->
  {Emigrants,Islands};
gather([I|T],Acc,Emigrants) ->
  N = misc_util:averageNumber(config:migrationProbability(),I),
  case N of
    0 ->
      gather(T,[I|Acc],Emigrants);
    _ ->
      {NewEmigrants,NewIsland} = lists:split(N,I),
      gather(T,[NewIsland|Acc],[{NewEmigrants,length(Acc)+1}|Emigrants])
  end.

-spec oneFightsRest(Agent::fighter(), ToFight::[fighter()], Fought::[fighter()]) -> {fighter(),[fighter()]}.
%% @doc Funkcja uruchamiajaca funkcje doFight/1 dla agenta A oraz
%% kazdego osobnika z listy ToFight. Agenci po walce przechowywani sa
%% w akumulatorze Fought i na koncu zwracani w krotce z agentem A po walkach.
oneFightsRest(Agent,[],Fought) -> {Agent,Fought};
oneFightsRest(Agent,[H|ToFight],Fought) ->
  [NewAgent,NewH]  = doFight({Agent,H}),
  oneFightsRest(NewAgent,ToFight,[NewH|Fought]).

-spec optionalPairs([agent()],[{agent(),agent()}]) -> [{agent(),agent()} | {agent()}].
%% @doc Funkcja dzielaca podana liste agentow na pary. Tail recursion.
optionalPairs([],Acc) -> Acc;
optionalPairs([A],Acc) -> [{A}|Acc];
optionalPairs([A,B|L],Acc) -> optionalPairs(L,[{A,B}|Acc]).