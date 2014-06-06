%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami dotyczacymi ewolucji czyli przechodzenia jednej generacji w kolejna.
%% Sa tu rowniez funkcje implementujace migracje miedzy wyspami
-module(evolution).
-export([sendToWork/1, doReproduce/1, doFight/1, eachFightsAll/1, optionalPairs/2]).

-include ("emas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Funkcja dostaje atom precyzujacy klase agentow i ich liste,
%% a nastepnie wykonuje odpowiednie operacje dla kazdej z klas.
%% Funkcja zwraca liste agentow po przetworzeniu.
-spec sendToWork({agent_behaviour(),[agent()]}) -> [agent()].
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

%% @doc Funkcja implementujaca walke "kazdy z kazdym" dla listy agentow w argumencie.
%% Zwracana jest lista agentow po walkach.
-spec eachFightsAll([agent()]) -> [agent()].
eachFightsAll([]) -> [];
eachFightsAll([H|T]) ->
    {NewH,NewT} = oneFightsRest(H,T,[]),
    [NewH | eachFightsAll(NewT)].

%% @doc Funkcja implementujaca logike "walki" pojedynczego agenta.
%% Zwracany jest ten sam agent w liscie.
-spec doFight({agent()} | {agent(),agent()}) -> [agent()].
doFight({A}) -> [A];
%% @doc Funkcja implementujaca logike walki dwoch agentow.
%% Zwracana jest lista dwoch przetworzonych agentow.
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
    AtoBtransfer =
        if EvA < EvB -> erlang:min(emas_config:fightTransfer(), EnA);
           EvA >= EvB -> -erlang:min(emas_config:fightTransfer(), EnB)
        end,
    [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].

%% @doc Funkcja implementujaca logike reprodukcji pojedynczego agenta.
%% Zwracanych jest dwoje agentow w liscie.
-spec doReproduce({agent()} | {agent(),agent()}) -> [agent()].
doReproduce({{SolA, EvA, EnA}}) ->
    SolB = genetic:reproduction(SolA),
    EvB = genetic:evaluation(SolB),
    AtoBtransfer = erlang:min(emas_config:reproductionTransfer(), EnA),
    [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
%% @doc Funkcja implementujaca logike reprodukcji dwoch agentow.
%% Zwracanych jest czterech agentow w liscie.
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
    [SolC, SolD] = genetic:reproduction(SolA, SolB),
    [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
    [AtoCTransfer, BtoDTransfer] = [ erlang:min(emas_config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
    [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja uruchamiajaca funkcje doFight/1 dla agenta A oraz
%% kazdego osobnika z listy ToFight. Agenci po walce przechowywani sa
%% w akumulatorze Fought i na koncu zwracani w krotce z agentem A po walkach.
-spec oneFightsRest(Agent::agent(), ToFight::[agent()], Fought::[agent()]) -> {agent(),[agent()]}.
oneFightsRest(Agent,[],Fought) -> {Agent,Fought};
oneFightsRest(Agent,[H|ToFight],Fought) ->
    [NewAgent,NewH]  = doFight({Agent,H}),
    oneFightsRest(NewAgent,ToFight,[NewH|Fought]).

%% @doc Funkcja dzielaca podana liste agentow na pary. Tail recursion.
-spec optionalPairs([agent()],[{agent(),agent()}]) -> [{agent(),agent()} | {agent()}].
optionalPairs([],Acc) -> Acc;
optionalPairs([A],Acc) -> [{A}|Acc];
optionalPairs([A,B|L],Acc) -> optionalPairs(L,[{A,B}|Acc]).