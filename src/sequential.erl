%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(sequential).
-export([run/3, run/0, run/1]).

%% ====================================================================
%% API functions
%% ====================================================================


run() ->
  run(40,5000,2).

run([A,B,C]) ->
  run(list_to_integer(A),
    list_to_integer(B),
    list_to_integer(C)).

run(ProblemSize,Time,Islands) ->
  random:seed(erlang:now()),
  {_Time,{_Result,FDs}} = timer:tc(fun start/3, [ProblemSize,Time,Islands]),
  [io_util:closeFiles(FDDict) || FDDict <- FDs].
  %io:format("Total time:   ~p s~nFitness:     ~p~n",[_Time/1000000,_Result]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec generate() -> List1
%% @doc Funkcja generujaca losowa liste agentow.
generate(ProblemSize) ->
  Solutions = [genetic:solution(ProblemSize) || _ <- lists:seq(1, config:populationSize())],
  [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions].

%% @spec start() -> float()
%% @doc Funkcja tworzaca odpowiednia ilosc wysp i przechodzaca do glownej petli.
%% Zwracany jest koncowy wynik.
start(ProblemSize,Time,IslandsNr) ->
  Islands = [generate(ProblemSize) || _ <- lists:seq(1,IslandsNr)],
  Path = io_util:genPath("Sequential",ProblemSize,Time,IslandsNr),
  FDs = [io_util:prepareWriting(filename:join([Path,"isl" ++ integer_to_list(N)])) || N <- lists:seq(1,IslandsNr)],
  timer:send_after(Time,theEnd),
  loop(Islands,FDs).

%% @spec loop(List1) -> float()
%% @doc Glowa petla programu. Gdy osiagnieta zostanie pozadana precyzja,
%% wynik jest zwracany.
loop(Islands,FDs) ->
  receive
    theEnd ->
      Best = lists:max([misc_util:result(I) || I <- Islands]),
      {Best,FDs}
  after 0 ->
    IslandsMigrated = evolution:doMigrate(Islands),
    Groups = [misc_util:groupBy(fun misc_util:behavior_noMig/1, I) || I <- IslandsMigrated],
    NewGroups = [lists:map(fun evolution:sendToWork/1,I) || I <- Groups],
    NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- NewGroups],
    io_util:writeIslands(FDs,NewIslands),
    %io_util:print(lists:max([misc_util:result(I) || I <- NewIslands])),
    loop(NewIslands,FDs)
  end.

