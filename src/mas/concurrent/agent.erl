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
start(Agent, Arenas, SimParams, Config) ->
    misc_util:seed_random(),
    loop(Agent, Arenas, SimParams, Config).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Defines a life cycle of a single agent
-spec loop(agent(), dict:dict(), config(), sim_params()) -> ok.
loop(Agent, Arenas, SimParams, Config) ->
    Environment = Config#config.agent_env,
    case Environment:behaviour_function(Agent, SimParams) of
        migration ->
            loop(Agent, port:emigrate(dict:fetch(migration, Arenas), Agent), SimParams, Config);
        Activity ->
            ArenaPid = dict:fetch(Activity, Arenas),
            case arena:call(ArenaPid, Agent) of
                close ->
                    ok;
                NewAgent ->
                    loop(NewAgent, Arenas, SimParams, Config)
            end
    end.