%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(sequential_lists).
-export([start/5, start/0, start/1]).

-record(counters,{fight = 0 :: non_neg_integer(),
  reproduction = 0 :: non_neg_integer(),
  migration = 0 :: non_neg_integer(),
  death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type island() :: [agent()].
-type counters() :: #counters{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> ok.
start() ->
  sequential:start(fun start/5).

-spec start(list()) -> ok.
start(Args) ->
  sequential:start(Args,fun start/5).

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path) ->
  sequential:start(ProblemSize,Time,Islands,Topology,Path,fun init/5).

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec init(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> {float(),[dict()]}.
%% @doc Funkcja tworzaca odpowiednia ilosc wysp i przechodzaca do glownej petli.
%% Zwracany jest koncowy wynik.
init(ProblemSize,Time,IslandsNr,Topology,Path) ->
  Islands = [genetic:generatePopulation(ProblemSize) || _ <- lists:seq(1,IslandsNr)],
  FDs = sequential:init(Time,IslandsNr,Topology,Path),
  loop(Islands,FDs,#counters{}).

-spec loop([island()],[dict()],counters()) -> {float(),[dict()]}.
%% @doc Glowa petla programu. Każda iteracja powoduje ewolucję nowej generacji osobnikow.
loop(Islands,FDs,Counters) ->
  receive
    {write,PreviousBest} ->
      io_util:writeIslands(FDs,Islands,Counters,PreviousBest),
      io_util:printSeq(Islands),
      timer:send_after(config:writeInterval(),{write,lists:max([misc_util:result(I) || I <- Islands])}),
      loop(Islands,FDs,[#counters{} || _ <- lists:seq(1,length(Islands))]);
    theEnd ->
      Best = lists:max([misc_util:result(I) || I <- Islands]),
      {Best,FDs}
  after 0 ->
    IslandsMigrated = evolution:doMigrate(Islands), % todo logowanie migracji
    Groups = [misc_util:groupBy([{misc_util:behavior_noMig(Agent),Agent} || Agent <- I]) || I <- IslandsMigrated],
    CountedIslands = [misc_util:countGroups(Island,#counters{}) || Island <- Groups],
    NewCounters = lists:foldl(fun misc_util:addCounters/2,Counters,CountedIslands),
    NewGroups = [lists:map(fun evolution:sendToWork/1,I) || I <- Groups],
    NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- NewGroups],
    loop(NewIslands,FDs,NewCounters)
  end.