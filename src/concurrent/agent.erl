%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/2, start/6]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Funkcja generujaca dane i startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
-spec start(pid(), pos_integer(), Ring::pid(), Bar::pid(), Port::pid(), Cemetery::pid()) -> ok.
start(Supervisor,ProblemSize,Ring,Bar,Port,Cemetery) ->
    misc_util:seedRandom(),
    Agent = genetic:generateAgent(ProblemSize),
    conc_supervisor:newAgent(Supervisor,Agent),
    loop(Agent,[Ring,Bar,Port,Cemetery]).

-spec start(agent(), Supervisor::pid()) -> ok.
start(Agent,Supervisor) ->
    random:seed(erlang:now()),
    loop(Agent,conc_supervisor:getArenas(Supervisor)).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja cyklu zycia agenta. Jego zachowanie jest zalezne od jego
%% energii. Rekurencja kreci sie w nieskonczonosc, poki energia nie osiagnie 0.
-spec loop(agent(),[pid()]) -> ok.
loop(Agent,Arenas) ->
    [Ring,Bar,Port,Cemetery] = Arenas,
    case misc_util:behavior(Agent) of
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
            loop(Agent,port:call(Port,Agent))
    end.