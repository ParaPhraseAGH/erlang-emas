%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(sequential_lists).
-export([start/4]).

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

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time,Islands,Topology,Path) ->
    %%     io:format("{Model=sequential_lists,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    topology:start_link(Islands,Topology),
    logger:start_link({sequential,Islands},Path),
    Environment = config:agent_env(),
    InitIslands = [Environment:initial_population() || _ <- lists:seq(1,Islands)],
    timer:send_after(Time,theEnd),
    timer:send_after(config:writeInterval(),write),
    {_Time,_Result} = timer:tc(fun loop/2, [InitIslands,#counter{}]),
    topology:close(),
    logger:close().

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
-spec loop([island()],counter()) -> float().
loop(Islands,Counter) ->
    Environment = config:agent_env(),
    receive
        write ->
            logger:logLocalStats(sequential,
                                 fitness,
                                 [misc_util:result(I) || I <- Islands]),
            logger:logLocalStats(sequential,
                                 population,
                                 [length(I) || I <- Islands]),
            logger:logGlobalStats(sequential,[{death,Counter#counter.death},
                                              {fight,Counter#counter.fight},
                                              {reproduction,Counter#counter.reproduction},
                                              {migration,Counter#counter.migration}]),
            %%             io_util:printSeq(Islands),
            timer:send_after(config:writeInterval(),write),
            loop(Islands,#counter{});
        theEnd ->
            lists:max([misc_util:result(I) || I <- Islands])
    after 0 ->
            Groups = [misc_util:groupBy([{Environment:behaviour_function(Agent),Agent} || Agent <- I]) || I <- Islands],
            Emigrants = [seq_migrate(lists:keyfind(migration,1,Island),Nr) || {Island,Nr} <- lists:zip(Groups,lists:seq(1,length(Groups)))],
            FlatEmigrants = lists:flatten(Emigrants),
            NewGroups = [[misc_util:meeting_proxy(Activity,sequential) || Activity <- I] || I <- Groups],
            WithEmigrants = append(FlatEmigrants,NewGroups),
            NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- WithEmigrants],
            NewCounter = countAllIslands(Groups,Counter),
            NrOfEmigrants = lists:foldl(fun({_To,AgentList},Acc) ->
                                                length(AgentList) + Acc
                                        end, 0, FlatEmigrants),
            loop(NewIslands,NewCounter#counter{migration = NrOfEmigrants + Counter#counter.migration})
    end.

%% @doc Liczy kategorie (ile fights,deaths etc.) na wszystkich wyspach i dodaje do Counter.
-spec countAllIslands([list()],counter()) -> counter().
countAllIslands(GroupedIslands,Counter) ->
    CountedIslands = [misc_util:countGroups(I,#counter{}) || I <- GroupedIslands],
    lists:foldl(fun misc_util:addCounters/2,Counter,CountedIslands).

seq_migrate(false,_) ->
    [];
seq_migrate({migration,Agents},From) ->
    Destinations = [{topology:getDestination(From),Agent} || Agent <- Agents],
    misc_util:groupBy(Destinations).

append([],Islands) ->
    Islands;
append([{Destination,Immigrants}|T],Islands) ->
    NewIslands = misc_util:mapIndex(Immigrants,Destination,Islands,fun lists:append/2),
    append(T,NewIslands).