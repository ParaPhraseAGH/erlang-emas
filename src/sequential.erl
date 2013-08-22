%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(sequential).
-export([run/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run() -> ok
%% @doc Funkcja uruchamiajaca algorytm dla wpisanych parametrow (config.erl)
run() ->
  random:seed(erlang:now()),
  {Time,{Result,FDs}} = timer:tc(fun start/0, []),
  [io_util:closeFiles(FDDict) || FDDict <- FDs],
  io:format("Total time:   ~p s~nFitness:     ~p~n",[Time/1000000,Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec generate() -> List1
%% @doc Funkcja generujaca losowa liste agentow.
generate() ->
  Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
  [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions].

%% @spec start() -> float()
%% @doc Funkcja tworzaca odpowiednia ilosc wysp i przechodzaca do glownej petli.
%% Zwracany jest koncowy wynik.
start() ->
  Islands = [generate() || _ <- lists:seq(1,config:islandsNr())],
  Instance = "instancja",
  file:make_dir(Instance),
  FDs = [io_util:prepareWriting(Instance ++ "\\" ++ integer_to_list(N)) || N <- lists:seq(1,config:islandsNr())],
  loop(Islands,FDs).

%% @spec loop(List1) -> float()
%% @doc Glowa petla programu. Gdy osiagnieta zostanie pozadana precyzja,
%% wynik jest zwracany.
loop(Islands,FDs) ->
  IslandsMigrated = evolution:doMigrate(Islands),
  Groups = [misc_util:groupBy(fun misc_util:behavior_noMig/1, I) || I <- IslandsMigrated],
  NewGroups = [lists:map(fun evolution:sendToWork/1,I) || I <- Groups],
  NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- NewGroups],
  Best = lists:max([misc_util:result(I) || I <- NewIslands]),
  io_util:writeIslands(FDs,NewIslands),
  %io_util:print(Result),
  Precision = config:stopPrec(),
  if Best >= -Precision ->
    {Best,FDs};
  Best < Precision ->
    loop(NewIslands,FDs)
  end.

