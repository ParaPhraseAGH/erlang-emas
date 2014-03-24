%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/2]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Funkcja startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
-spec start(agent(),[pid()]) -> ok.
start(Agent,Arenas) ->
    misc_util:seedRandom(),
    loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja cyklu zycia agenta. Jego zachowanie jest zalezne od jego
%% energii. Rekurencja kreci sie w nieskonczonosc, poki energia nie osiagnie 0.
-spec loop(agent(),[pid()]) -> ok.
loop(Agent,Arenas) ->
    [Ring,Bar,Port,Cemetery] = Arenas,
    Environment = config:agent_env(),
    case Environment:behaviour_function(Agent) of
        death ->
            cemetery:cast(Cemetery);
        reproduction ->
            {Solution,Fitness,_} = Agent,
            NewEnergy = bar:call(Bar,Agent),
            loop({Solution,Fitness,NewEnergy},Arenas);
        fight ->
            {Solution,Fitness,_} = Agent,
            NewEnergy = ring:call(Ring,Agent),
            loop({Solution,Fitness,NewEnergy},Arenas);
        migration ->
            loop(Agent,port:emigrate(Port,Agent))
    end.