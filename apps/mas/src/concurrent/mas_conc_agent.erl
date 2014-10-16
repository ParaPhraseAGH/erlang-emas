%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles logic of a single agent

-module(mas_conc_agent).
-export([start/4]).

-include("mas.hrl").

-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Initializes given agent on arenas given as parameter
-spec start(agent(), dict:dict(string(), pid()), sim_params(), config()) -> ok.
start(Agent, Arenas, SP, Cf) ->
    mas_misc_util:seed_random(),
    loop(Agent, Arenas, SP, Cf).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Defines a life cycle of a single agent
-spec loop(agent(), dict:dict(), sim_params(), config()) -> ok.
loop(Agent, Arenas, SP, Cf) ->
    case mas_misc_util:behaviour_proxy(Agent, SP, Cf) of
        migration ->
            loop(Agent, mas_conc_port:emigrate(dict:fetch(migration, Arenas), Agent), SP, Cf);
        Activity ->
            ArenaPid = dict:fetch(Activity, Arenas),
            case mas_conc_arena:call(ArenaPid, Agent) of
                close ->
                    ok;
                the_end ->
                    mas_concurrent:send_result(Agent);
                NewAgent ->
                    loop(NewAgent, Arenas, SP, Cf)
            end
    end.
