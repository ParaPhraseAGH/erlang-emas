%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles logic of a single agent

-module(agent).
-export([start/4]).

-include("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Initializes given agent on arenas given as parameter
-spec start(agent(), dict:dict(string(), pid()), sim_params(), config()) -> ok.
start(Agent, Arenas, SP, Cf) ->
    misc_util:seed_random(),
    loop(Agent, Arenas, SP, Cf).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Defines a life cycle of a single agent
-spec loop(agent(), dict:dict(), sim_params(), config()) -> ok.
loop(Agent, Arenas, SP, Cf) ->
    Environment = Cf#config.agent_env,
    case Environment:behaviour_function(Agent, SP) of
        migration ->
            loop(Agent, port:emigrate(dict:fetch(migration, Arenas), Agent), SP, Cf);
        Activity ->
            ArenaPid = dict:fetch(Activity, Arenas),
            case arena:call(ArenaPid, Agent) of
                close ->
                    ok;
                NewAgent ->
                    loop(NewAgent, Arenas, SP, Cf)
            end
    end.