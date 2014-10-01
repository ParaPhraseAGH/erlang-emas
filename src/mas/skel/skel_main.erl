%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1

-module(skel_main).
-export([start/4]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time,Islands,Topology,Path) ->
    topology:start_link(self(), Islands, Topology),
    skel_logger:start_link(Path),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    Environment = config:agent_env(),
    InitPopulation = [lists:map(fun(A) -> {I, A} end, Environment:initial_population()) || I <- lists:seq(1, Islands)],
    FlattenPopulation = lists:flatten(InitPopulation),
    {_Time, _Result} = timer:tc(fun main/2, [FlattenPopulation, Time]),
    topology:close(),
    skel_logger:close().
%%     io:format("Total time:   ~p s~nFitness:     ~p~n", [_Time / 1000000, _Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO add generic statistics (funstats)
%% @doc Main program loop
-spec main([tuple()], non_neg_integer()) -> float().
main(Population, Time) ->
    Environment = config:agent_env(),
    EndTime = misc_util:add_miliseconds(os:timestamp(), Time),
    Workers = 4,

    Tag = {seq, fun({Home, Agent}) ->
                        {{Home, Environment:behaviour_function(Agent)}, Agent}
                end},

    Migrate = {seq, fun _Migration({{Home, migration}, Agent}) ->
                            dict:from_list([{{topology:getDestination(Home), migration}, [Agent]}]);
                        _Migration({{Home, Behaviour}, Agent}) ->
                            dict:from_list([{{Home, Behaviour}, [Agent]}])
                    end},

    Group = {reduce,
             fun(D1, D2) ->
                     dict:merge(fun(_Key, Value1, Value2) ->
                                        Value1 ++ Value2
                                end, D1, D2)
             end,
             fun(X) -> X end},

    Unpack = {seq, fun dict:to_list/1},
    Log = {seq, fun(Chunks) ->
                        Counter = misc_util:create_new_counter(),
                        Counts = misc_util:add_interactions_to_counter([{B, A} || {{_H, B}, A} <- Chunks], Counter),
                        skel_logger:report_result(fight, dict:fetch(fight, Counts)),
                        skel_logger:report_result(reproduce, dict:fetch(reproduction, Counts)),
                        skel_logger:report_result(death, dict:fetch(death, Counts)),
                        skel_logger:report_result(migration, dict:fetch(migration, Counts)),
                        Chunks
                end},

    Work = {seq, fun({{Home, Behaviour}, Agents}) ->
                         NewAgents = Environment:meeting_function({Behaviour, Agents}),
                         [{Home, A} || A <- NewAgents]
                 end },

    Shuffle = {seq, fun(Agents) ->
                            misc_util:shuffle(lists:flatten(Agents))
                    end},

    Workflow = {pipe, [{map, [Tag, Migrate], Workers},
                       Group,
                       Unpack,
                       Log,
                       {map, [Work], Workers},
                       Shuffle]},

    [_FinalIslands] = skel:do([{feedback,
                                [Workflow],
                                _While = fun(Agents) ->
                                                 Fitness = lists:max([Fitness || {_, {_, Fitness, _}} <- Agents]),
                                                 PopulationSize = length(Agents),
                                                 skel_logger:report_result(fitness, Fitness),
                                                 skel_logger:report_result(population, PopulationSize),
                                                 os:timestamp() < EndTime
                                         end}],
                              [Population]),
    result.
