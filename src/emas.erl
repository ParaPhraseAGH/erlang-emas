%% @author krzywick
%% @doc @todo Add description to emas.


-module(emas).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0]).

run() -> 
	Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
	Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
	Result = step(Agents, config:steps()),
	lists:max([ Ev || {_ ,Ev, _} <- Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

behavior({_, _, Energy}) -> 
	case Energy of
		N when N == 0 -> death;
		N when N > 10 -> reproduction;
		_ -> fight
	end.

logic({Arena, Agents}) ->
	case Arena of
		death -> [];
		fight -> lists:flatmap(fun doFight/1, emas_util:optionalPairs(Agents));
		reproduction -> lists:flatmap(fun doReproduce/1, emas_util:optionalPairs(Agents))
	end.

doFight({A}) -> [A];
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) -> 
	AtoBtransfer = 
		if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
		   EvA > EvB -> -erlang:min(config:fightTransfer(), EnB);
		   EvA == EvB -> 0
		end,
	[{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].

doReproduce({{SolA, EvA, EnA}}) ->
	SolB = genetic:reproduction(SolA),
	EvB = genetic:evaluation(SolB),
	AtoBtransfer = erlang:min(config:reproductionTransfer(), EnA),
	[{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) -> 
	[SolC, SolD] = genetic:reproduction(SolA, SolB),
	[EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
	[AtoCTransfer, BtoDTransfer] = [ erlang:min(config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
	[{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].

step(Agents, 0) -> 
%% 	io:format("Population at step ~B: ~w ~n", [0, Agents]),
	Agents;
step(Agents, N) ->
%% 	io:format("Population at step ~B: ~w ~n", [N, Agents]),
	Groups = dict:to_list(emas_util:groupBy(fun behavior/1, Agents)),
	NewGroups = [ logic(G) || G <- Groups],
	NewAgents = emas_util:shuffle(lists:flatten(NewGroups)),
	step(NewAgents, N - 1).

