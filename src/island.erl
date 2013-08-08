%% Copyright
-module(island).
-author("jasiek").

%% API
-export([proces/0]).

%% ====================================================================
%% API functions
%% ====================================================================
proces() ->
  random:seed(erlang:now()),
  Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
  Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
  loop(Agents,0).

%% ====================================================================
%% Internal functions
%% ====================================================================

sendToWork({death, _}) ->
  [];
sendToWork({fight, Agents}) ->
  lists:flatmap(fun doFight/1, emas_util:optionalPairs(Agents));
sendToWork({reproduction,Agents}) ->
  lists:flatmap(fun doReproduce/1, emas_util:optionalPairs(Agents));
sendToWork({migration,Agents}) ->
  case Agents of
    [] -> [];
    [H|T] ->
      whereis(supervisor) ! {agent,self(),H},
      sendToWork({migration,T})
  end.

doFight({A}) -> [A];
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  AtoBtransfer =
    if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
      EvA >= EvB -> -erlang:min(config:fightTransfer(), EnB)
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

loop(Agents,Counter) ->
%	io:format("Population at step ~B: ~w ~n", [config:steps() - N + 1, Agents]),
  WithImmigrants = emas_util:addImmigrants(Agents),
% emas_util:energyReport(N,WithImmigrants),
  Groups = emas_util:regroup(WithImmigrants),
  NewGroups = [sendToWork(G) || G <- Groups],
  NewAgents = emas_util:shuffle(lists:flatten(NewGroups)),
  Result = emas_util:result(NewAgents),
  whereis(supervisor) ! {result,Result},
  if Counter == 100 ->
    Counter2 = 0,
    emas_util:print(Result,Groups);
    Counter /= 100 ->
      Counter2 = Counter + 1
  end,
  loop(NewAgents, Counter2).