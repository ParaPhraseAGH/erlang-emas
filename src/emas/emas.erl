-module (emas).
-behaviour(agent_env).

-export ([starts/1, start/2, start/3, initial_population/0, behaviour_function/1, behaviours/0, meeting_function/1, stats/0]).

% type model() is defined in mas.hrl
% emas.hrl included by mas.hrl
-include ("mas.hrl").

-spec start(model(),pos_integer()) -> ok.
start(Model, Time) ->
    mas:start(?MODULE,Model,Time,[]).

-spec starts([list()]) -> ok.
starts(Args) ->
    [Model, Time] = Args,
    mas:start(?MODULE,erlang:list_to_atom(Model),erlang:list_to_integer(Time),[]).

-spec start(model(),pos_integer(),[tuple()]) -> ok.
start(Model, Time, Options) ->
    mas:start(?MODULE,Model,Time,Options).

-spec initial_population() -> [agent()].
initial_population() ->
    genetic:generatePopulation(emas_config:problemSize()).

%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie jego energii.
-spec behaviour_function(agent()) -> agent_behaviour().
behaviour_function({_,_,0}) ->
    death;

behaviour_function({_, _, Energy}) ->
    case random:uniform() < emas_config:migrationProbability() of
        true -> migration;
        false -> case Energy > emas_config:reproductionThreshold() of
                     true -> reproduction;
                     false -> fight
                 end
    end.


-spec behaviours() -> [agent_behaviour()].
behaviours() ->
    [reproduction, death, fight, migration].

-spec meeting_function({agent_behaviour(), [agent()]}) -> [agent()].
meeting_function({death, _}) ->
    [];

meeting_function({reproduction, Agents}) ->
    lists:flatmap(fun evolution:doReproduce/1, evolution:optionalPairs(Agents,[]));

meeting_function({fight, Agents}) ->
    lists:flatmap(fun evolution:doFight/1, evolution:optionalPairs(Agents,[]));

meeting_function({migration, Agents}) ->
    Agents;

meeting_function({_, _}) ->
    erlang:error(unexpected_behaviour).

-spec stats() -> [funstat()].
stats() ->
    Fitness_map = fun({_Solution,Fitness,_Energy}) ->
                              Fitness
                      end,
    Fitness_reduce = fun(F1, F2) ->
                             lists:max([F1,F2])
                     end,
    [{fitness, Fitness_map, Fitness_reduce, -999999}].
