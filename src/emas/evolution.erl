%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc A module with evolutionary functions which transform one generation into another, including migrations.
-module(evolution).
-export([sendToWork/1, doReproduce/1, doFight/1, eachFightsAll/1, optionalPairs/2]).

-include ("emas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc This function receives an atom representing the type of the agents and a list of agents. It executes
%% the appropriate action on every agent according to its type and returns the agregated results.
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

%% @doc This function implements an all-vs-all fight between agents. It returns the list of agents after the fight.
-spec eachFightsAll([agent()]) -> [agent()].
eachFightsAll([]) -> [];
eachFightsAll([H|T]) ->
    {NewH,NewT} = oneFightsRest(H,T,[]),
    [NewH | eachFightsAll(NewT)].

%% @doc The fight logic for a pair of agents. It returns a list of both agents updated after the fight.
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

%% @doc The reproduction logic for a single agent. It returns a list with the updated parent and new child.
-spec doReproduce({agent()} | {agent(),agent()}) -> [agent()].
doReproduce({{SolA, EvA, EnA}}) ->
    SolB = genetic:reproduction(SolA),
    EvB = genetic:evaluation(SolB),
    AtoBtransfer = erlang:min(emas_config:reproductionTransfer(), EnA),
    [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
%% @doc The reproduction logic for a pair of agents. It returns a list with the updated parents and new children.
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
    [SolC, SolD] = genetic:reproduction(SolA, SolB),
    [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
    [AtoCTransfer, BtoDTransfer] = [ erlang:min(emas_config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
    [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Executes the doFight/1 function between agent A and every other agent from the ToFight list.
-spec oneFightsRest(Agent::agent(), ToFight::[agent()], Fought::[agent()]) -> {agent(),[agent()]}.
oneFightsRest(Agent,[],Fought) -> {Agent,Fought};
oneFightsRest(Agent,[H|ToFight],Fought) ->
    [NewAgent,NewH]  = doFight({Agent,H}),
    oneFightsRest(NewAgent,ToFight,[NewH|Fought]).

%% @doc Splits agents into pairs with an optional single remainder.
-spec optionalPairs([agent()],[{agent(),agent()}]) -> [{agent(),agent()} | {agent()}].
optionalPairs([],Acc) -> Acc;
optionalPairs([A],Acc) -> [{A}|Acc];
optionalPairs([A,B|L],Acc) -> optionalPairs(L,[{A,B}|Acc]).