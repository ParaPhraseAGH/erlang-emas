-module (emas).
-behaviour(agent_env).

-export ([starts/1, start/2, start/3, initial_population/0, behaviour_function/1, behaviours/0, meeting_function/1, stats/0]).

-include ("mas.hrl").

%% ====================================================================
%% Callbacks
%% ====================================================================

-spec start(model(),pos_integer()) -> ok.
start(Model, Time) ->
    mas:start(?MODULE, Model, Time, load_params(), []).


-spec start(model(),pos_integer(),[tuple()]) -> ok.
start(Model, Time, Options) ->
    mas:start(?MODULE, Model, Time, load_params(), Options).


-spec starts(list()) -> ok.
starts([Model, Time]) ->
    mas:start(?MODULE, erlang:list_to_atom(Model), erlang:list_to_integer(Time), load_params(), []).


-spec initial_population() -> [agent()].
initial_population() ->
    genetic:generatePopulation(emas_config:problemSize()).


%% @doc This function chooses a behaviour for the agent based on its energy.
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

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec load_params() -> sim_params().
load_params() ->
    {ok, ParamsFromFile} = file:consult("etc/emas.config"),
    proplist_to_record(ParamsFromFile).

%% @doc Transform a proplist with simulation properties to a record
-spec proplist_to_record([tuple()]) -> sim_params().
proplist_to_record(Proplist) ->
    Dict = dict:from_list(Proplist),
    #sim_params{genetic_ops = dict:fetch(genetic_ops,Dict),
                problem_size = dict:fetch(problem_size,Dict),
                monitor_diversity = dict:fetch(monitor_diversity,Dict),
                initial_energy = dict:fetch(initial_energy,Dict),
                reproduction_threshold = dict:fetch(reproduction_threshold,Dict),
                reproduction_transfer = dict:fetch(reproduction_transfer,Dict),
                fight_transfer = dict:fetch(fight_transfer,Dict),
                mutation_rate = dict:fetch(mutation_rate,Dict),
                mutation_range = dict:fetch(mutation_range,Dict),
                mutation_chance = dict:fetch(mutation_chance,Dict),
                migration_probability = dict:fetch(migration_probability,Dict),
                recombination_chance = dict:fetch(recombination_chance,Dict),
                fight_number = dict:fetch(fight_number,Dict)}.