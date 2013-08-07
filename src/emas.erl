%% @author krzywick
%% @doc @todo Add description to emas.


-module(emas).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0,run/1,start/0]).

run() ->
  run(1).

run(NoIslands) ->
  register(supervisor,self()),
  PidsRefs = [spawn_monitor(emas,start,[]) || _ <- lists:seq(1,NoIslands)],
  {Pids,_} = lists:unzip(PidsRefs),
  receiver(NoIslands,Pids).

start() ->
  random:seed(erlang:now()),
  Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
  Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
  {Time,Result} = timer:tc(fun step/2, [Agents,config:steps()]),
  whereis(supervisor) ! {result,Time,Result}.

%% ====================================================================
%% Internal functions
%% ====================================================================

receiver(0,_) ->
  unregister(supervisor),
  ok;
receiver(N,Pids) ->
  receive
    {result,Time,Result} ->
      io:format("~nTotal time: ~p s ~nBest fitness: ~p~n",[Time/1000000,Result]),
      receiver(N,Pids);
    {agent,_From,Agent} ->
      Index = random:uniform(length(Pids)),  % a co jesli lista pusta? (jest jeden proces)
      lists:nth(Index, Pids) ! {agent,self(),Agent},
      receiver(N,Pids);
    {'DOWN',_Ref,process,Pid,Reason} ->
      case Reason of
        'Populacja wymarla' ->
          io:format("Populacja wymarla w procesie ~p~n",[Pid]);
        normal ->
          ok;
        _ ->
          io:format("Proces ~p zakonczyl sie z powodu ~p~n",[Pid,Reason])
      end,
      receiver(N-1,lists:delete(Pid,Pids))
  after 10000 ->
    unregister(supervisor),
    timeout
  end.

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

step(Agents, 0) ->
	lists:max([ Fitness || {_ ,Fitness, _} <- Agents]);
step(Agents, N) ->
%	io:format("Population at step ~B: ~w ~n", [config:steps() - N + 1, Agents]),
  WithImmigrants = emas_util:addImmigrants(Agents),
	Groups = emas_util:regroup(WithImmigrants),
	NewGroups = [sendToWork(G) || G <- Groups],
	NewAgents = emas_util:shuffle(lists:flatten(NewGroups)),
  if NewAgents == [] ->
      exit('Populacja wymarla');
    NewAgents /= [] -> notyet
  end,
%	emas_util:print(N,NewAgents,Groups),
	step(NewAgents, N - 1).