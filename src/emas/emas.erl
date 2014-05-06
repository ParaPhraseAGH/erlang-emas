-module (emas).
-behaviour(agent_env).

-export ([start/3, initial_population/0, behaviour_function/1, behaviours/0, meeting_function/1, stats/0]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type agent_behaviour() :: death | reproduction | fight | migration.

-type model() :: sequential_lists | hybrid | concurrent.

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

-spec stats() -> [{atom(),fun(),term()}].
stats() ->
    Fitness = fun(Agent,YetBest) ->
                      {_,F,_} = Agent,
                      lists:max([F,YetBest])
              end,
    Energy = fun(Agent,Sum) ->
                     {_,_,E} = Agent,
                     E + Sum
             end,
    [{fitness,Fitness,-999999},{energy,Energy,0}].