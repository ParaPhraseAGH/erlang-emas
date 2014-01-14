%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/4, start/5]).
-record(arenas,{fight,reproduction,migration}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type arenas() :: #arenas{}.

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Funkcja generujaca dane i startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
-spec start(pid(), pos_integer(), Ring::pid(), Bar::pid(), Port::pid()) -> no_return().
start(Supervisor,ProblemSize,Ring,Bar,Port) when is_integer(ProblemSize) ->
    misc_util:seedRandom(),
    Agent = genetic:generateAgent(ProblemSize),
    conc_supervisor:newAgent(Supervisor,Agent),
    Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
    loop(Agent,Arenas).
%% @doc Funkcja startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac oraz dane agenta.
-spec start(agent(), Ring::pid(), Bar::pid(), Port::pid()) -> no_return().
start(Agent,Ring,Bar,Port)  when is_tuple(Agent)  ->
    random:seed(erlang:now()),
    Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
    loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja cyklu zycia agenta. Jego zachowanie jest zalezne od jego
%% energii. Rekurencja kreci sie w nieskonczonosc, poki energia nie osiagnie 0.
-spec loop(agent(),arenas()) -> no_return().
loop(Agent,Arenas) ->
    case misc_util:behavior(Agent) of
        death ->
            exit(dying);
        reproduction ->
            {Solution,Fitness,_} = Agent,
            NewEnergy = bar:call(Arenas#arenas.reproduction,Agent),
            loop({Solution,Fitness,NewEnergy},Arenas);
        fight ->
            {Solution,Fitness,_} = Agent,
            NewEnergy = ring:call(Arenas#arenas.fight,Agent),
            loop({Solution,Fitness,NewEnergy},Arenas);
        migration ->
            [Ring,Bar,Port] = port:call(Arenas#arenas.migration,Agent),
            loop(Agent,#arenas{fight = Ring, reproduction = Bar, migration = Port})
    end.