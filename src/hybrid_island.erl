%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy.

-module(hybrid_island).
-export([proces/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec proces() -> loop(List)
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia
%% petle, w ktorej porusza sie proces.
proces(Path,N,ProblemSize) ->
  random:seed(erlang:now()),
  IslandPath = filename:join([Path,"isl" ++ integer_to_list(N)]),
  FDs = io_util:prepareWriting(IslandPath),
  Solutions = [genetic:solution(ProblemSize) || _ <- lists:seq(1, config:populationSize())],
  Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
  timer:send_after(config:writeInterval(),write),
  loop(Agents,FDs).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec loop(List1) -> loop(List2)
%% @doc Glowna petla procesu. Kazda iteracja powoduje obliczenie
%% kolejnego wyniku i opcjonalnie wyslanie go do supervisora.
loop(Agents,FDs) ->
  receive
    write ->
      io_util:write(dict:fetch(fitness,FDs),misc_util:result(Agents)),
      io_util:write(dict:fetch(population,FDs),length(Agents)),
      io:format("Island ~p Fitness ~p Population ~p~n",[self(),misc_util:result(Agents),length(Agents)]),
      timer:send_after(config:writeInterval(),write),
      loop(Agents,FDs);
    {agent,_Pid,A} ->
      loop([A|Agents],FDs);
    {finish,_Pid} ->
      io_util:closeFiles(FDs)
  after 0 ->
    Groups = misc_util:groupBy(fun misc_util:behavior/1, Agents),
    NewGroups = [evolution:sendToWork(G) || G <- Groups],
    NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),
    %io_util:print(Result,Groups),
    loop(NewAgents,FDs)
  end.