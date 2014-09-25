%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc A module with evolutionary functions which transform one generation into another, including migrations.
-module(evolution).
-export([sendToWork/1, do_reproduce/2, do_fight/2, eachFightsAll/1, optional_pairs/2]).

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
-spec do_fight({agent()} | {agent(), agent()}, sim_params()) -> [agent()].
do_fight({A}, _SimParams) -> [A];

%% @doc Funkcja implementujaca logike walki dwoch agentow.
%% Zwracana jest lista dwoch przetworzonych agentow.
do_fight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}, SimParams) ->
    AtoBtransfer =
        if EvA < EvB -> erlang:min(SimParams#sim_params.fight_transfer, EnA);
           EvA >= EvB -> -erlang:min(SimParams#sim_params.fight_transfer, EnB)
        end,
    [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].


%% @doc The reproduction logic for a single agent. It returns a list with the updated parent and new child.
-spec do_reproduce({agent()} | {agent(), agent()}, sim_params()) -> [agent()].
do_reproduce({{SolA, EvA, EnA}}, SimParams) ->
    SolB = genetic:reproduction(SolA, SimParams),
    EvB = genetic:evaluation(SolB, SimParams),
    AtoBtransfer = erlang:min(SimParams#sim_params.reproduction_transfer, EnA),
    [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];

%% @doc The reproduction logic for a pair of agents. It returns a list with the updated parents and new children.
do_reproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}, SimParams) ->
    [SolC, SolD] = genetic:reproduction(SolA, SolB, SimParams),
    [EvC, EvD] = [genetic:evaluation(S, SimParams) || S <- [SolC, SolD]],
    [AtoCTransfer, BtoDTransfer] = [erlang:min(SimParams#sim_params.reproduction_transfer, E) || E <- [EnA, EnB]],
    [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].


%% @doc Splits agents into pairs with an optional single remainder.
-spec optional_pairs([agent()],[{agent(),agent()}]) -> [{agent(),agent()} | {agent()}].
optional_pairs([],Acc) -> Acc;

optional_pairs([A],Acc) -> [{A}|Acc];

optional_pairs([A,B|L],Acc) -> optional_pairs(L,[{A,B}|Acc]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Executes the doFight/1 function between agent A and every other agent from the ToFight list.
-spec oneFightsRest(Agent::agent(), ToFight::[agent()], Fought::[agent()]) -> {agent(),[agent()]}.
oneFightsRest(Agent,[],Fought) -> {Agent,Fought};

oneFightsRest(Agent,[H|ToFight],Fought) ->
    [NewAgent,NewH]  = doFight({Agent,H}),
    oneFightsRest(NewAgent,ToFight,[NewH|Fought]).