%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles the logic of a single island in hybrid model

-module(hybrid_island).
-export([start/2, close/1, sendAgent/2]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Generates initial data and starts the computation
-spec start(sim_params(), config()) -> ok.
start(SimParams, Config = #config{agent_env = Environment}) ->
    misc_util:seed_random(),
    Agents = misc_util:generate_population(SimParams, Config),
    timer:send_interval(Config#config.write_interval, write),
    loop(Agents, misc_util:create_new_counter(Config), Environment:stats(), SimParams, Config).

-spec close(pid()) -> {finish, pid()}.
close(Pid) ->
    Pid ! {finish, self()}.


%% @doc Asynchronusly sends an agent immigrating to this island
-spec sendAgent(pid(), agent()) -> {agent, pid(), agent()}.
sendAgent(Pid, Agent) ->
    Pid ! {agent, self(), Agent}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc The main island process loop. A new generation of the population is created in every iteration.
-spec loop([agent()], counter(), [tuple()], sim_params(), config()) -> ok.
loop(Agents, InteractionCounter, Funstats, SimParams, Config) ->
    Environment = Config#config.agent_env,
    receive
        write ->
            [logger:log_countstat(self(), Interaction, Val) || {Interaction, Val} <- dict:to_list(InteractionCounter)],
            [logger:log_funstat(self(), StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- Funstats],
            loop(Agents, misc_util:create_new_counter(Config), Funstats, SimParams, Config);
        {agent, _Pid, A} ->
            loop([A|Agents], InteractionCounter, Funstats, SimParams, Config);
        {finish, _Pid} ->
            ok
    after 0 ->
            Groups = misc_util:group_by([{Environment:behaviour_function(A, SimParams), A} || A <- Agents ]),
            NewGroups = [misc_util:meeting_proxy(G, hybrid, SimParams, Config) || G <- Groups],
            NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),

            NewFunstats = misc_util:count_funstats(NewAgents, Funstats),
            NewCounter = misc_util:add_interactions_to_counter(Groups, InteractionCounter),

            loop(NewAgents, NewCounter, NewFunstats, SimParams, Config)
    end.
