%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc A module with evolutionary functions which transform one generation into another, including migrations.
-module(evolution).
-export([send_to_work/2, do_reproduce/2, do_fight/2, optional_pairs/2]).

-include ("emas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc This function receives an atom representing the type of the agents and a list of agents. It executes
%% the appropriate action on every agent according to its type and returns the agregated results.
-spec send_to_work({agent_behaviour(),[agent()]}, sim_params()) -> [agent()].
send_to_work({death, _}, SimParams) -> %% TODO change this function
    [];

send_to_work({fight, Agents}, SimParams) ->
    lists:flatmap(fun do_fight/2, optional_pairs(Agents,[]));

send_to_work({reproduction, Agents}, SimParams) ->
    lists:flatmap(fun do_reproduce/2, optional_pairs(Agents,[]));

send_to_work({migration,[]}, _SimParams) ->
    [];

send_to_work({migration,[Agent|T]}, SimParams) when tuple_size(Agent) == 3 ->
    hybrid:sendAgent(Agent),
    send_to_work({migration,T}, SimParams);

send_to_work({migration,[{From,Agent}|T]}, SimParams) ->
    [{topology:getDestination(From),Agent} | send_to_work({migration,T}, SimParams)].


%% @doc This function implements an all-vs-all fight between agents. It returns the list of agents after the fight.
%% -spec each_fights_all([agent()]) -> [agent()].
%% each_fights_all([]) -> [];
%%
%% each_fights_all([H|T]) ->
%%     {NewH,NewT} = one_fights_rest(H,T,[]),
%%     [NewH | each_fights_all(NewT)].


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
%%
%% %% @doc Executes the doFight/1 function between agent A and every other agent from the ToFight list.
%% -spec one_fights_rest(Agent::agent(), ToFight::[agent()], Fought::[agent()]) -> {agent(),[agent()]}.
%% one_fights_rest(Agent,[],Fought) -> {Agent,Fought};
%%
%% one_fights_rest(Agent,[H|ToFight],Fought) ->
%%     [NewAgent,NewH]  = do_fight({Agent,H}),
%%     one_fights_rest(NewAgent,ToFight,[NewH|Fought]).