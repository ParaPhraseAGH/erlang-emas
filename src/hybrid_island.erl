%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy.

-module(hybrid_island).
-export([proces/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec proces() -> loop(List)
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia
%% petle, w ktorej porusza sie proces.
proces(Instancja,N) ->
  random:seed(erlang:now()),
  FDs = io_util:prepareWriting(Instancja ++ "\\" ++ integer_to_list(N)),
  Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
  Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
  loop(Agents,FDs).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec loop(List1) -> loop(List2)
%% @doc Glowna petla procesu. Kazda iteracja powoduje obliczenie
%% kolejnego wyniku i opcjonalnie wyslanie go do supervisora.
loop(Agents,FDs) ->
  receive
    {agent,_Pid,A} ->
      loop([A|Agents],FDs);
    {finish,_Pid} ->
      io_util:closeFiles(FDs)
  after 0 ->
    Groups = misc_util:groupBy(fun misc_util:behavior/1, Agents),
    NewGroups = [evolution:sendToWork(G) || G <- Groups],
    NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),
    Result = misc_util:result(NewAgents),
    io_util:write(dict:fetch(fitness,FDs),Result),
    io_util:write(dict:fetch(population,FDs),length(NewAgents)),
    if Result /= islandEmpty ->
      whereis(supervisor) ! {result,Result};
      Result == islandEmpty ->
        donothing
    end,
    %io_util:print(Result,Groups),
    loop(NewAgents,FDs)
  end.