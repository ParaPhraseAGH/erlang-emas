%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc This is the main module of sequential model. It handles starting the system and cleaning after work

-module(mas_sequential).
-export([start/3]).

-include ("mas.hrl").

-type agent() :: mas:agent().

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), sim_params(), config()) -> [agent()].
start(Time, SP, Cf = #config{islands = Islands, agent_env = Env}) ->
    mas_misc_util:seed_random(),
    mas_misc_util:clear_inbox(),
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    InitIslands = [mas_misc_util:generate_population(SP, Cf) || _ <- lists:seq(1, Islands)],
    mas_logger:start_link(lists:seq(1, Islands), Cf),
    timer:send_after(Time, the_end),
    {ok, TRef} = timer:send_interval(Cf#config.write_interval, write),
    {_Time, Result} = timer:tc(fun loop/5, [InitIslands,
                                            [mas_misc_util:create_new_counter(Cf) || _ <- lists:seq(1, Islands)],
                                            [Env:stats() || _ <- lists:seq(1, Islands)],
                                            SP,
                                            Cf]),
    timer:cancel(TRef),
    mas_topology:close(),
    mas_logger:close(),
    Result.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc The main island process loop. A new generation of the population is created in every iteration.
-spec loop([island()], [counter()], [funstat()], sim_params(), config()) -> float().
loop(Islands, Counters, Funstats, SP, Cf) ->
    receive
        write ->
            [log_island(Nr, C, F) || {Nr, C, F} <- lists:zip3(lists:seq(1, length(Islands)), Counters, Funstats)],
            loop(Islands,
                 [mas_misc_util:create_new_counter(Cf) || _ <- lists:seq(1, length(Islands))],
                 Funstats,
                 SP,
                 Cf);
        the_end ->
            lists:flatten(Islands)
    after 0 ->
            Groups = [mas_misc_util:group_by([{mas_misc_util:behaviour_proxy(Agent, SP, Cf), Agent} || Agent <- I]) || I <- Islands],
            Emigrants = [seq_migrate(lists:keyfind(migration, 1, Island), Nr) || {Island, Nr} <- lists:zip(Groups, lists:seq(1, length(Groups)))],
            NewGroups = [[mas_misc_util:meeting_proxy(Activity, sequential, SP, Cf) || Activity <- I] || I <- Groups],
            WithEmigrants = append(lists:flatten(Emigrants), NewGroups),
            NewIslands = [mas_misc_util:shuffle(lists:flatten(I)) || I <- WithEmigrants],

            NewCounters = [mas_misc_util:add_interactions_to_counter(G, C) || {G, C} <- lists:zip(Groups, Counters)],
            NewFunstats = [mas_misc_util:count_funstats(I, F) || {I, F} <- lists:zip(NewIslands, Funstats)],

            loop(NewIslands, NewCounters, NewFunstats, SP, Cf)
    end.

-spec log_island(pos_integer(), counter(), [funstat()]) -> [ok].
log_island(Key, Counter, Funstats) ->
    [mas_logger:log_countstat(Key, Interaction, Val) || {Interaction, Val} <- dict:to_list(Counter)],
    [mas_logger:log_funstat(Key, StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- Funstats].


-spec seq_migrate(false | {migration,[agent()]}, pos_integer()) -> [{migration,[agent()]}].
seq_migrate(false,_) ->
    [];

seq_migrate({migration,Agents},From) ->
    Destinations = [{mas_topology:getDestination(From),Agent} || Agent <- Agents],
    mas_misc_util:group_by(Destinations).


-spec append({pos_integer(),[agent()]}, [list(agent())]) -> [list(agent())].
append([],Islands) ->
    Islands;

append([{Destination,Immigrants}|T],Islands) ->
    NewIslands = mas_misc_util:map_index(Immigrants,Destination,Islands,fun lists:append/2),
    append(T,NewIslands).
