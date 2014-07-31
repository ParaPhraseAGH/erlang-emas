%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles logic of a single agent

-module(agent).
-export([start/2]).

-type agent() :: term().

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Initializes given agent on arenas given as parameter
-spec start(agent(),dict:dict(string(), pid())) -> ok.
start(Agent,Arenas) ->
    misc_util:seedRandom(),
    loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Defines a life cycle of a single agent
-spec loop(agent(),dict:dict()) -> ok.
loop(Agent,Arenas) ->
    Environment = config:agent_env(),
    case Environment:behaviour_function(Agent) of
        migration ->
            loop(Agent,port:emigrate(dict:fetch(migration,Arenas),Agent));
        Activity ->
            ArenaPid = dict:fetch(Activity,Arenas),
            case arena:call(ArenaPid,Agent) of
                close ->
                    ok;
                NewAgent ->
                    loop(NewAgent,Arenas)
            end
    end.