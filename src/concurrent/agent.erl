%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/4]).
-record(arenas,{fight,reproduction,migration}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type arenas() :: #arenas{}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(pos_integer() | agent(), Ring::pid(), Bar::pid(), Port::pid()) -> no_return().
%% @doc Funkcja generujaca dane i startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
start(ProblemSize,Ring,Bar,Port) when is_integer(ProblemSize) ->
  misc_util:seedRandom(),
  Agent = genetic:generateAgent(ProblemSize),
  Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
  loop(Agent,Arenas);
%% @doc Funkcja startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac oraz dane agenta.
start(Agent,Ring,Bar,Port)  when is_tuple(Agent)  ->
  random:seed(erlang:now()),
  Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
  loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec loop(agent(),arenas()) -> no_return().
%% @doc Funkcja cyklu zycia agenta. Jego zachowanie jest zalezne od jego
%% energii. Rekurencja kreci sie w nieskonczonosc, poki energia nie osiagnie 0.
loop(Agent,Arenas) ->
  case misc_util:behavior(Agent) of
    death ->
      exit(dying);
    reproduction ->
      {Solution,Fitness,_} = Agent,
      NewEnergy = bar:call(Arenas#arenas.reproduction,Agent),%arenas:call(Agent,Arenas#arenas.reproduction),
      loop({Solution,Fitness,NewEnergy},Arenas);
    fight ->
      {Solution,Fitness,_} = Agent,
      NewEnergy = ring:call(Arenas#arenas.fight,Agent),%arenas:call(Agent,Arenas#arenas.fight),
      loop({Solution,Fitness,NewEnergy},Arenas);
    migration ->
      [Ring,Bar,Port] = port:call(Arenas#arenas.migration), %arenas:call(emigration,Arenas#arenas.migration),
      loop(Agent,#arenas{fight = Ring, reproduction = Bar, migration = Port})
  end.