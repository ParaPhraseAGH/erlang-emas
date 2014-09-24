%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1

-module(skel_lists).
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
    InitIslands = [Environment:initial_population() || _ <- lists:seq(1, Islands)],
    {_Time, _Result} = timer:tc(fun main/2, [InitIslands,Time]),
    topology:close(),
    skel_logger:close().
%%     io:format("Total time:   ~p s~nFitness:     ~p~n", [_Time / 1000000, _Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO add generic statistics (funstats)
%% @doc Main program loop
-spec main([island()], non_neg_integer()) -> float().
main(Islands, Time) ->
    Environment = config:agent_env(),
    EndTime = misc_util:add_miliseconds(os:timestamp(), Time),
    %%     Tag = {seq, fun(Island) ->
    %%                         [{Environment:behaviour_function(Agent), Agent} || Agent <- Island] %% bylo behaviour no_mig
    %%                 end},

    Tag = {map, [{seq, fun(Agent) ->
                               {Environment:behaviour_function(Agent), Agent} %% bylo behaviour no_mig
                       end}]},

    Group = {seq, fun misc_util:groupBy/1},

    Log = {seq, fun(Island) ->
                        Counter = misc_util:createNewCounter(),
                        Counts = misc_util:add_interactions_to_counter(Island,Counter),
                        skel_logger:report_result(fight, dict:fetch(fight,Counts)),
                        skel_logger:report_result(reproduce, dict:fetch(reproduction,Counts)),
                        skel_logger:report_result(death, dict:fetch(death,Counts)),
                        Island
                end},

    Work = {map,
            [{seq,fun Environment:meeting_function/1}],
            4},

    Shuffle = {seq, fun(Island) ->
                            misc_util:shuffle(lists:flatten(Island))
                    end},

    OneRun = {map,
              [{pipe, [Tag,
                       Group,
                       Log,
                       Work,
                       Shuffle]}]},

    Migration = {seq, fun(AllIslands) ->
                              {NrOfEmigrants, IslandsMigrated} = do_migrate(AllIslands),
                              skel_logger:report_result(migration, NrOfEmigrants),
                              IslandsMigrated
                      end},

    [FinalIslands] = skel:do([{feedback,
                               [OneRun,
                                Migration],
                               _While = fun(List) ->
                                                Fitness = lists:max([misc_util:result(Island) || Island <- List]),
                                                Population = lists:sum([length(Island) || Island <- List]),
                                                skel_logger:report_result(fitness, Fitness),
                                                skel_logger:report_result(population, Population),
                                                os:timestamp() < EndTime
                                        end}],
                             [Islands]),
    %%     io_util:printSeq(FinalIslands),
    misc_util:result(lists:flatten(FinalIslands)).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO maybe enhance migration
%% @doc Function responsible for migration. For each island statistical number of agents is chosen to emigrate.
%% These agents are added to particular islands and new islands are returned.
-spec do_migrate([island()]) -> {non_neg_integer(), [island()]}.
do_migrate(Islands) ->
    {Gathered, NewIslands} = gather(Islands, [], []),
    {length(Gathered), append(Gathered, lists:reverse(NewIslands))}.


%% @doc Function assigns a new islands to each agent group and adds the agents to the population
-spec append([{[agent()], integer()}], [island()]) -> [island()].
append([], Islands) -> Islands;

append([{Immigrants, From}|T], Islands) ->
    Destination = topology:getDestination(From),
    NewIslands = misc_util:mapIndex(Immigrants, Destination, Islands, fun lists:append/2),
    append(T, NewIslands).


%% @doc Function determines how many agents should emigrate and extracts them from the islands.
%% The emigrant list is returned with the new (smaller) list of islands.
-spec gather([island()], [island()], [{[agent()], integer()}]) -> {[{[agent()], integer()}], [island()]}.
gather([], Islands, Emigrants) ->
    {Emigrants, Islands};

gather([I|T], Acc, Emigrants) ->
    N = misc_util:averageNumber(emas_config:migrationProbability(), I),
    case N of
        0 ->
            gather(T, [I|Acc], Emigrants);
        _ ->
            {NewEmigrants, NewIsland} = lists:split(N, I),
            gather(T, [NewIsland|Acc], [{NewEmigrants, length(Acc) + 1}|Emigrants])
    end.
