%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles the logic of a single island in hybrid model

-module(hybrid_island).
-export([start/0, close/1, sendAgent/2]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Generates initial data and starts the computation
-spec start() -> ok.
start() ->
    misc_util:seedRandom(),
    Environment = config:agent_env(),
    Agents = Environment:initial_population(),
    timer:send_interval(config:writeInterval(), write),
    loop(Agents, misc_util:createNewCounter(), Environment:stats()).

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
-spec loop([agent()], counter(), [tuple()]) -> ok.
loop(Agents, InteractionCounter, Funstats) ->
    Environment = config:agent_env(),
    receive
        write ->
            [logger:log_countstat(self(), Interaction, Val) || {Interaction, Val} <- dict:to_list(InteractionCounter)],
            [logger:log_funstat(self(), StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- Funstats],
            loop(Agents, misc_util:createNewCounter(), Funstats);
        {agent, _Pid, A} ->
            loop([A|Agents], InteractionCounter, Funstats);
        {finish, _Pid} ->
            ok
    after 0 ->
            Groups = misc_util:groupBy([{Environment:behaviour_function(A), A} || A <- Agents ]),
            NewGroups = [misc_util:meeting_proxy(G, hybrid) || G <- Groups],
            NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),

            NewFunstats = misc_util:count_funstats(NewAgents, Funstats),
            NewCounter = misc_util:add_interactions_to_counter(Groups, InteractionCounter),

            loop(NewAgents, NewCounter, NewFunstats)
    end.
