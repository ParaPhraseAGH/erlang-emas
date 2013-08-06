%% @author krzywick
%% @doc @todo Add description to emas_util.


-module(emas_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([shuffle/1, optionalPairs/1, print/3, behavior/1, regroup/1, isUniform/2]).

regroup(Agents) ->
	DeathList = [X || X <- Agents, behavior(X) == death],
	FightList = [X || X <- Agents, behavior(X) == fight],
	ReproductionList = [X || X <- Agents, behavior(X) == reproduction],
	[{death,DeathList},{fight,FightList},{reproduction,ReproductionList}].

shuffle(L) ->
	Rand = [{random:uniform(), N} || N <- L],
	[X||{_,X} <- lists:sort(Rand)].

optionalPairs(L) ->
	optionalPairsTail(L,[]).

isUniform(Groups,Step) ->
	[{death,D},{fight,_},{reproduction,R}] = Groups,
	Ld = length(D),
	Lr = length(R),
	AllSteps = config:steps(),
	if Step == AllSteps -> false;
		Ld + Lr == 0 -> true;
		Ld + Lr /= 0 -> false
	end.

print(Step,Agents,Groups) ->
	[{death,D},{fight,F},{reproduction,R}] = Groups,
	Fitness = lists:max([ Ev || {_ ,Ev, _} <- Agents]),
	if Step rem 100 == 0 ->
		    io:format("~nStep ~p, Fitness: ~p~n",[config:steps() - Step,Fitness]),
		    io:format("Died: ~p    Fought: ~p    Reproduced: ~p~n",[length(D),length(F),length(R)]);
	   Step rem 100 /= 0 ->
		    notyet
	end.

behavior({_, _, Energy}) -> 
	ReproductionThreshold = config:reproductionThreshold(),
	if  Energy == 0 -> death;
		Energy >  ReproductionThreshold -> reproduction;
		Energy =< ReproductionThreshold -> fight
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

optionalPairsTail([],Acc) -> Acc;
optionalPairsTail([A],Acc) -> [{A}|Acc];
optionalPairsTail([A,B|L],Acc) -> optionalPairsTail(L,[{A,B}|Acc]).
