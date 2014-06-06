%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/2]).

-type agent() :: term().

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Funkcja startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
-spec start(agent(),dict:dict()) -> ok.
start(Agent,Arenas) ->
    misc_util:seedRandom(),
    loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja cyklu zycia agenta.
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