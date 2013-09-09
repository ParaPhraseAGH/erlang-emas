%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(sequential).
-export([start/5, start/0, start/1, generate/1]).

%% ====================================================================
%% API functions
%% ====================================================================


start() ->
  file:make_dir("tmp"),
  start(40,5000,2,mesh,"tmp").

start([A,B,C,D,E]) ->
  start(list_to_integer(A),
    list_to_integer(B),
    list_to_integer(C),
    list_to_atom(D),E).

start(ProblemSize,Time,Islands,Topology,Path) ->
  random:seed(erlang:now()),
  misc_util:clearInbox(),
  {_Time,{_Result,FDs}} = timer:tc(fun init/5, [ProblemSize,Time,Islands,Topology,Path]),
  [io_util:closeFiles(FDDict) || FDDict <- FDs],
  topology:close(),
  io:format("Total time:   ~p s~nFitness:     ~p~n",[_Time/1000000,_Result]).


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
init(ProblemSize,Time,IslandsNr,Topology,Path) ->
  Islands = [generate(ProblemSize) || _ <- lists:seq(1,IslandsNr)],
  %Path = io_util:genPath("Sequential",ProblemSize,Time,IslandsNr),
  FDs = [io_util:prepareWriting(filename:join([Path,"isl" ++ integer_to_list(N)])) || N <- lists:seq(1,IslandsNr)],
  timer:send_after(Time,theEnd),
  timer:send_after(config:writeInterval(),write),
  topology:start_link(IslandsNr,Topology),
  loop(Islands,FDs).

%% @spec loop(List1) -> float()
%% @doc Glowa petla programu. Gdy osiagnieta zostanie pozadana precyzja,
%% wynik jest zwracany.
loop(Islands,FDs) ->
  receive
    write ->
      io_util:writeIslands(FDs,Islands),
      io_util:printSeq(Islands),
      timer:send_after(config:writeInterval(),write),
      loop(Islands,FDs);
    theEnd ->
      Best = lists:max([misc_util:result(I) || I <- Islands]),
      {Best,FDs}
  after 0 ->
    IslandsMigrated = evolution:doMigrate(Islands),
    Groups = [misc_util:groupBy(fun misc_util:behavior_noMig/1, I) || I <- IslandsMigrated],
    NewGroups = [lists:map(fun evolution:sendToWork/1,I) || I <- Groups],
    NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- NewGroups],
    loop(NewIslands,FDs)
  end.

