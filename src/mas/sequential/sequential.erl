%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(sequential).
-export([start/4]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time, Islands, Topology, Path) ->
    %%     io:format("{Model=sequential,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    topology:start_link(self(), Islands, Topology),
    Environment = config:agent_env(),
    InitIslands = [Environment:initial_population() || _ <- lists:seq(1, Islands)],
    logger:start_link(lists:seq(1, Islands), Path),
    timer:send_after(Time, theEnd),
    {ok, TRef} = timer:send_interval(config:writeInterval(), write),
    {_Time,_Result} = timer:tc(fun loop/2, [InitIslands,[misc_util:createNewCounter() || _ <- lists:seq(1, Islands)]]),
    timer:cancel(TRef),
    topology:close(),
    logger:close().

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
-spec loop([island()], [counter()]) -> float().
loop(Islands, Counters) ->
    Environment = config:agent_env(),
    receive
        write ->
            Environment = config:agent_env(),
            Funstats = Environment:stats(),
            %% TODO InitVal in funstats is never overwritten!
            [log(Nr, I, C, Funstats) || {Nr, I, C} <- lists:zip3(lists:seq(1, length(Islands)), Islands, Counters)],
            loop(Islands, [misc_util:createNewCounter() || _ <- lists:seq(1, length(Islands))]);
        theEnd ->
            lists:max([misc_util:result(I) || I <- Islands])
    after 0 ->
        %% TODO update funstats on every iteration!
        Groups = [misc_util:groupBy([{Environment:behaviour_function(Agent),Agent} || Agent <- I]) || I <- Islands],
        NewCounters = [misc_util:add_interactions_to_counter(G, C) || {G, C} <- lists:zip(Groups, Counters)],
        Emigrants = [seq_migrate(lists:keyfind(migration, 1, Island), Nr) || {Island, Nr} <- lists:zip(Groups, lists:seq(1, length(Groups)))],
        NewGroups = [[misc_util:meeting_proxy(Activity, sequential) || Activity <- I] || I <- Groups],
        WithEmigrants = append(lists:flatten(Emigrants), NewGroups),
        NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- WithEmigrants],
        loop(NewIslands,NewCounters)
    end.

-spec log(pos_integer(), [agent()], counter(), [funstat()]) -> [ok].
log(Nr, _Island, Counter, []) ->
    [logger:log_countstat(Nr, Stat, Val) || {Stat, Val} <- dict:to_list(Counter)];

log(Nr, Island, Counter, [{StatName, MapFun, ReduceFun, InitVal}|Stats]) ->
    StatVal = lists:foldl(ReduceFun,
                          InitVal,
                          [MapFun(Agent) || Agent <- Island]),
    logger:log_funstat(Nr, StatName, StatVal),
    log(Nr, Island, Counter, Stats).


-spec seq_migrate(false | {migration,[agent()]}, pos_integer()) -> [{migration,[agent()]}].
seq_migrate(false,_) ->
    [];

seq_migrate({migration,Agents},From) ->
    Destinations = [{topology:getDestination(From),Agent} || Agent <- Agents],
    misc_util:groupBy(Destinations).


-spec append({pos_integer(),[agent()]}, [list(agent())]) -> [list(agent())].
append([],Islands) ->
    Islands;

append([{Destination,Immigrants}|T],Islands) ->
    NewIslands = misc_util:mapIndex(Immigrants,Destination,Islands,fun lists:append/2),
    append(T,NewIslands).