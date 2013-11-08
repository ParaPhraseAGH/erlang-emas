%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(sequential_lists).
-export([start/5, start/0, start/1]).

-record(counter,{fight = 0 :: non_neg_integer(),
                 reproduction = 0 :: non_neg_integer(),
                 migration = 0 :: non_neg_integer(),
                 death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type island() :: [agent()].
-type counter() :: #counter{}.

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
-spec init(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> float().
%% @doc Funkcja tworzaca odpowiednia ilosc wysp i przechodzaca do glownej petli.
%% Zwracany jest koncowy wynik.
init(ProblemSize,Time,IslandsNr,Topology,Path) ->
    Islands = [genetic:generatePopulation(ProblemSize) || _ <- lists:seq(1,IslandsNr)],
    sequential:init(Time,IslandsNr,Topology,Path),
    loop(Islands,#counter{}).

-spec loop([island()],counter()) -> float().
%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
loop(Islands,Counter) ->
    receive
        write ->
            logger:logGlobalStats(sequential,{Counter#counter.death,
                                              Counter#counter.fight,
                                              Counter#counter.reproduction,
                                              Counter#counter.migration}),
            logger:logLocalStats(sequential,
                                 fitness,
                                 [misc_util:result(I) || I <- Islands]),
            logger:logLocalStats(sequential,
                                 population,
                                 [length(I) || I <- Islands]),
            io_util:printSeq(Islands),
            timer:send_after(config:writeInterval(),write),
            loop(Islands,#counter{});
        theEnd ->
            lists:max([misc_util:result(I) || I <- Islands])
    after 0 ->
            {NrOfEmigrants,IslandsMigrated} = evolution:doMigrate(Islands),
            Groups = [misc_util:groupBy([{misc_util:behavior_noMig(Agent),Agent} || Agent <- I]) || I <- IslandsMigrated],
            NewGroups = [lists:map(fun evolution:sendToWork/1,I) || I <- Groups],
            NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- NewGroups],
            NewCounter = countAllIslands(Groups,Counter),
            loop(NewIslands,NewCounter#counter{migration = NrOfEmigrants + Counter#counter.migration})
    end.

-spec countAllIslands([list()],counter()) -> counter().
%% @doc Liczy kategorie (ile fights,deaths etc.) na wszystkich wyspach i dodaje do Counter.
countAllIslands(GroupedIslands,Counter) ->
    CountedIslands = [misc_util:countGroups(I,#counter{}) || I <- GroupedIslands],
    lists:foldl(fun misc_util:addCounters/2,Counter,CountedIslands).
