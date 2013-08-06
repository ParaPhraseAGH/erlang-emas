%% @author krzywick
%% @doc @todo Add description to emas_util.


-module(emas_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([shuffle/1, optionalPairs/1, print/3, behavior/1, regroup/1, isUniform/2, addImmigrants/1]).

regroup(Agents) ->
	DeathList = [X || X <- Agents, behavior(X) == death],
	FightList = [X || X <- Agents, behavior(X) == fight],
	ReproductionList = [X || X <- Agents, behavior(X) == reproduction],
  MigrationList = [X || X <- Agents, behavior(X) == migration],
	[{death,DeathList},{fight,FightList},{reproduction,ReproductionList},{migration,MigrationList}].

shuffle(L) ->
	Rand = [{random:uniform(), N} || N <- L],
	[X||{_,X} <- lists:sort(Rand)].

addImmigrants(Agents) ->
  Pid = whereis(supervisor),
  receive
    {agent,Pid,A} -> addImmigrants([A|Agents])
  after 0 ->
    Agents
  end.

optionalPairs(L) ->
	optionalPairsTail(L,[]).

%% Aktualnie nieuzywana funkcja do sprawdzania identycznosci kolejnych generacji
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
	[{death,D},{fight,F},{reproduction,R},{migration,M}] = Groups,
	Fitness = lists:max([ Ev || {_ ,Ev, _} <- Agents]),
	if Step rem 100 == 0 ->
		    io:format("~nProcess: ~p, Step ~p, Fitness: ~p~n",[self(),config:steps() - Step,Fitness]),
		    io:format("Died: ~p    Fought: ~p    Reproduced: ~p    Migrated: ~p~n",[length(D),length(F),length(R),length(M)]);
	   Step rem 100 /= 0 ->
		    notyet
	end.

behavior({_,_,0}) ->
  death;
behavior({_, _, Energy}) ->
  case random:uniform() < config:migrationProbability() of
    true -> migration;
    false -> case Energy > config:reproductionThreshold() of
               true -> reproduction;
               false -> fight
             end
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

optionalPairsTail([],Acc) -> Acc;
optionalPairsTail([A],Acc) -> [{A}|Acc];
optionalPairsTail([A,B|L],Acc) -> optionalPairsTail(L,[{A,B}|Acc]).
