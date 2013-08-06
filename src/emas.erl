%% @author krzywick
%% @doc @todo Add description to emas.


-module(emas).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0]).

run() -> 
	random:seed(erlang:now()),
	Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
	Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
	{Time,Result} = timer:tc(fun step/2, [Agents,config:steps()]),
	io:format("Total time: ~p s ~nBest fitness: ~p~n",[Time/1000000,Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================


sendToWork({death, _}) ->
	[];
sendToWork({fight, Agents}) ->
	lists:flatmap(fun doFight/1, emas_util:optionalPairs(Agents));
sendToWork({reproduction,Agents}) ->
	lists:flatmap(fun doReproduce/1, emas_util:optionalPairs(Agents)).

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
	lists:max([ Ev || {_ ,Ev, _} <- Agents]);
step(Agents, N) ->
%% 	io:format("Population at step ~B: ~w ~n", [config:steps() - N + 1, Agents]),
	Groups = emas_util:regroup(Agents),
	NewGroups = [sendToWork(G) || G <- Groups],
	NewAgents = emas_util:shuffle(lists:flatten(NewGroups)),
	emas_util:print(N,NewAgents,Groups),
	case emas_util:isUniform(Groups,N) of
		true -> step(NewAgents, 0);
		false -> step(NewAgents, N - 1)
	end.
