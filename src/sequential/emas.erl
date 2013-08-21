%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(emas).
-export([run/0,doMigrate/1,generate/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run() -> ok
%% @doc Funkcja uruchamiajaca algorytm dla wpisanych parametrow (config.erl)
run() ->
  random:seed(erlang:now()),
  {Time,Result} = timer:tc(fun start/0, []),
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
  loop(Islands).

%% @spec loop(List1) -> float()
%% @doc Glowa petla programu. Gdy osiagnieta zostanie pozadana precyzja,
%% wynik jest zwracany.
loop(Islands) ->
  IslandsMigrated = doMigrate(Islands),
  Groups = [emas_util:groupBy(fun emas_util:behavior/1, I) || I <- IslandsMigrated],
  NewGroups = [lists:map(fun sendToWork/1,I) || I <- Groups ],
  NewIslands = [emas_util:shuffle(lists:flatten(I)) || I <- NewGroups],
  Result = emas_util:result(lists:append(NewIslands)),
  %emas_util:print(Result),
  Precision = config:stopPrec(),
  if Result >= -Precision ->
    Result;
  Result < Precision ->
    loop(NewIslands)
  end.

%% @spec doMigrate(List1) -> List2
%% @doc Funkcja dokonujaca migracji. Wyznaczana jest liczba agentow,
%% ktorzy powinni ulec przesiedleniu, dokonywana migracja i zwracana
%% przetworzona lista wysp.
doMigrate(Islands) ->
  IslandEmigrants = config:migrationProbability() * config:populationSize(),
  if IslandEmigrants == 0 ->
    Islands;
  IslandEmigrants < 1 ->
    case random:uniform() < IslandEmigrants of
      true -> migrate(1,Islands);
      false -> Islands
    end;
  IslandEmigrants >= 1 ->
    migrate(trunc(IslandEmigrants),Islands)
  end.

%% @spec migrate(int(),List1) -> List2
%% @doc Funkcja przesiedlacjaca N osobnikow z kazdej wyspy na inna
%% (moze to byc ta sama wyspa). Zwracana jest przetworzona lista wysp.
migrate(N,Islands) ->
  {Agents,NewIslands} = lists:unzip([lists:split(N,I)|| I <- Islands, I /= []]),
  Shuffled = emas_util:shuffle(lists:append(Agents)),
  emas_util:multiAppend(N,Shuffled,NewIslands).

%% @spec sendToWork({atom(),List1}) -> List2
%% @doc Funkcja dostaje atom precyzujacy klase agentow i ich liste,
%% a nastepnie wykonuje odpowiednie operacje dla kazdej z klas.
%% Funkcja zwraca liste agentow po przetworzeniu.
sendToWork({death, _}) ->
  [];
sendToWork({fight, Agents}) ->
  lists:flatmap(fun doFight/1, emas_util:optionalPairs(Agents));
sendToWork({reproduction,Agents}) ->
  lists:flatmap(fun doReproduce/1, emas_util:optionalPairs(Agents)).

%% @spec doFight({Agent1}) -> [Agent2]
%% @doc Funkcja implementujaca logike "walki" pojedynczego agenta.
%% Zwracany jest ten sam agent w liscie.
doFight({A}) -> [A];
%% @spec doFight({Agent1,Agent2}) -> [Agent3,Agent4]
%% @doc Funkcja implementujaca logike walki dwoch agentow.
%% Zwracana jest lista dwoch przetworzonych agentow.
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  AtoBtransfer =
    if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
      EvA >= EvB -> -erlang:min(config:fightTransfer(), EnB)
    end,
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].

%% @spec doReproduce({Agent1}) -> List
%% @doc Funkcja implementujaca logike reprodukcji pojedynczego agenta.
%% Zwracana jest dwojka agentow w liscie.
doReproduce({{SolA, EvA, EnA}}) ->
  SolB = genetic:reproduction(SolA),
  EvB = genetic:evaluation(SolB),
  AtoBtransfer = erlang:min(config:reproductionTransfer(), EnA),
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
%% @spec doReproduce({Agent1,Agent2}) -> [Agent3,Agent4,Agent5,Agent6]
%% @doc Funkcja implementujaca logike reprodukcji dwoch agentow.
%% Zwracanych jest czterech agentow w liscie.
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  [SolC, SolD] = genetic:reproduction(SolA, SolB),
  [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
  [AtoCTransfer, BtoDTransfer] = [ erlang:min(config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
  [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].

