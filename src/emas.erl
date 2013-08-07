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
  receiver(Pids).

start() ->
  random:seed(erlang:now()),
  Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
  Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
  loop(Agents).
  %{Time,Result} = timer:tc(fun step/2, [Agents,config:steps()]),

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Powinien byc jeszcze timeout
receiver(Pids) ->
  receive
    {result,Result} ->
      Precision = config:stopPrec(),
      if Result == islandEmpty ->
        receiver(Pids);
      Result < -Precision ->
        receiver(Pids);
      Result >= -Precision ->
        io:format("Final result: ~p~n",[Result]),
        emas_util:cleanup(Pids)
      end;
%    {energy,From,Energy} ->
%      case lists:member(From,EnList) of
%        true ->
%          case lists:sort(EnList) of
%            Pids ->
%              io:format("Suma energii wynosi: ~p~n",[EnSum]),
%              self() ! {energy,From,Energy},
%              receiver(N,Pids,[],0);
%            _ ->
%              self() ! {energy,From,Energy},
%              receiver(N,Pids,EnList,EnSum)
%          end;
%        false ->
%          receiver(N,Pids,[From|EnList],Energy + EnSum)
%      end;
    {agent,_From,Agent} ->
      Index = random:uniform(length(Pids)),  % a co jesli lista pusta? (jest jeden proces)
      lists:nth(Index, Pids) ! {agent,self(),Agent},
      receiver(Pids);
    {'DOWN',_Ref,process,Pid,Reason} ->
      case Reason of
        normal ->
          ok;
        _ ->
          io:format("Proces ~p zakonczyl sie z powodu ~p~n",[Pid,Reason])
      end,
      %% Tutaj mozna postawic kolejna wyspe
      receiver(lists:delete(Pid,Pids))
  after 10000 ->
    emas_util:cleanup(Pids),
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

loop(Agents) ->
%	io:format("Population at step ~B: ~w ~n", [config:steps() - N + 1, Agents]),
  WithImmigrants = emas_util:addImmigrants(Agents),
% emas_util:energyReport(N,WithImmigrants),
	Groups = emas_util:regroup(WithImmigrants),
	NewGroups = [sendToWork(G) || G <- Groups],
	NewAgents = emas_util:shuffle(lists:flatten(NewGroups)),
%	emas_util:print(N,NewAgents,Groups),
  whereis(supervisor) ! {result,emas_util:result(NewAgents)},
	loop(NewAgents).