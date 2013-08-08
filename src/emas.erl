%% @author krzywick
%% @doc @todo Add description to emas.


-module(emas).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0,run/1]).

run() ->
  run(1).

run(NoIslands) ->
  init(),
  {Time,{Result,Pids}} = timer:tc(fun spawner/1, [NoIslands]),
  cleanup(Pids),
  io:format("Total time:   ~p s~nFitness:     ~p~n",[Time/1000000,Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
  register(supervisor,self()).

cleanup(Pids) ->
  emas_util:rambo(Pids),
  emas_util:checkIfDead(Pids),
  emas_util:clearInbox(),
  unregister(supervisor).

spawner(NoIslands) ->
  PidsRefs = [spawn_monitor(island,proces,[]) || _ <- lists:seq(1,NoIslands)],
  {Pids,_} = lists:unzip(PidsRefs),
  receiver(Pids).

%% Powinien byc jeszcze timeout
receiver(Pids) ->
  receive
    {result,Result} ->
      Precision = config:stopPrec(),
      if Result == islandEmpty orelse Result < -Precision ->
        receiver(Pids);
      Result >= -Precision ->
        {Result,Pids}
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
      Index = random:uniform(length(Pids)),
      lists:nth(Index, Pids) ! {agent,self(),Agent},
      receiver(Pids);
    {'DOWN',_Ref,process,Pid,Reason} ->
      case Reason of
        _ ->
          io:format("Proces ~p zakonczyl sie z powodu ~p~n",[Pid,Reason])
      end,
      {NewPid,_Ref} = spawn_monitor(emas,proces,[]),
      io:format("Stawiam kolejna wyspe o Pid ~p~n",[NewPid]),
      receiver([NewPid|lists:delete(Pid,Pids)])
  after 10000 ->
    cleanup(Pids),
    timeout
  end.

