%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1

-module(skel_main).
-export([start/3,
         seed_random_once_per_process/0]).

-include ("mas.hrl").

-compile([{inline,[ seed_random_once_per_process/0]}]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), sim_params(), config()) -> ok.
start(Time, SP, Cf = #config{islands = Islands, agent_env = Env}) ->
    topology:start_link(self(), Islands, Cf#config.topology),
    logger:start_link(lists:seq(1, Cf#config.islands), Cf),
    misc_util:seed_random(),
    misc_util:clear_inbox(),
    Population = [{I, Env:initial_agent(SP)} ||
                     _ <- lists:seq(1, Cf#config.population_size),
                     I <- lists:seq(1, Islands)],
    {_Time, Result} = timer:tc(fun main/4, [Population, Time, SP, Cf]),
    topology:close(),
    logger:close(),
    Result.
%%     io:format("Total time:   ~p s~nFitness:     ~p~n", [_Time / 1000000, _Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Main program loop
-spec main([tuple()], non_neg_integer(), sim_params(), config()) -> float().
main(Population, Time, SP, Cf) ->
    EndTime = misc_util:add_miliseconds(os:timestamp(), Time),
    Workers = Cf#config.skel_workers,

    TagFun = fun({Home, Agent}) ->
                     seed_random_once_per_process(),
                     {{Home, misc_util:behaviour_proxy(Agent, SP, Cf)}, Agent}
             end,

    MigrateFun = fun({{Home, migration}, Agent}) ->
                         {{topology:getDestination(Home), migration}, Agent};
                    (OtherAgent)->
                         OtherAgent
                 end,

    GroupFun = fun misc_util:group_by/1,

    LogFun = fun(Groups) ->
                     log_countstats(Groups, Cf),
                     log_funstats(Groups, Cf),
                     Groups
             end,


    TMGL = fun (Agents) ->
                   Tagged = lists:map(TagFun,
                                      Agents),
                   Migrated = lists:map(MigrateFun,
                                        Tagged),
                   Grouped = GroupFun(Migrated),
                   LogFun(Grouped)
           end,


    Work = {seq, fun({{Home, Behaviour}, Agents}) ->
                         seed_random_once_per_process(),
                         NewAgents = misc_util:meeting_proxy({Behaviour, Agents}, skel, SP, Cf),
                         [{Home, A} || A <- NewAgents]
                 end },

    Shuffle = {seq, fun(Agents) ->
                            seed_random_once_per_process(),
                            misc_util:shuffle(lists:flatten(Agents))
                    end},

    Workflow = {pipe, [{seq, TMGL},
                       {map, [Work], Workers},
                       Shuffle]},

    [FinalIslands] = skel:do([{feedback,
                                [Workflow],
                                _While = fun(_Agents) ->
                                                 os:timestamp() < EndTime
                                         end}],
                              [Population]),
    _FinalAgents =
        [Agent || {_Island, Agent} <- FinalIslands].


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec log_countstats([tuple()], config()) -> ok.
log_countstats(Groups, Cf) ->
    BigDict = dict:from_list([{I, misc_util:create_new_counter(Cf)}
                              || I <- lists:seq(1, Cf#config.islands)]),

    NewBigDict = lists:foldl(fun({{Home, Behaviour}, Group}, AccBD) ->
                                     IslandDict = dict:fetch(Home, AccBD),
                                     NewIslandDict = dict:update_counter(Behaviour, length(Group), IslandDict),
                                     dict:store(Home, NewIslandDict, AccBD)
                             end, BigDict, Groups),

    [[logger:log_countstat(Island, Stat, Val)
      || {Stat, Val} <- dict:to_list(Counter)]
     || {Island, Counter} <- dict:to_list(NewBigDict)],

    ok.

-spec log_funstats([tuple()], config()) -> ok.
log_funstats(Groups, Cf) ->
    Env = Cf#config.agent_env,
    FunstatDict = dict:from_list([{I, Env:stats()}
                                  || I <- lists:seq(1, Cf#config.islands)]),

    NewDict = lists:foldl(fun({{Home, _Beh}, Agents}, Dict) ->
                                  Funstats = dict:fetch(Home, Dict),
                                  NewFunstats = misc_util:count_funstats(Agents, Funstats),
                                  dict:store(Home, NewFunstats, Dict)
                          end, FunstatDict, Groups),

    [[logger:log_funstat(Home, Stat, Val)
      || {Stat, _Map, _Reduce, Val} <- Stats]
     || {Home, Stats} <- dict:to_list(NewDict)],

    ok.



seed_random_once_per_process() ->
    case get(was_seeded) of
        undefined ->
            misc_util:seed_random(),
            put(was_seeded, true);
        true ->
            ok
    end.
