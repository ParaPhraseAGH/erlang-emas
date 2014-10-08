-module (emas).
-behaviour(agent_env).

-export ([starts/1, start/2, start/3, start/4, initial_agent/1, behaviour_function/2, behaviours/0, meeting_function/2, stats/0]).

-include ("mas.hrl").

-define(LOAD(Prop, Dict), Prop = dict:fetch(Prop,Dict)).

%% ====================================================================
%% Callbacks
%% ====================================================================

-spec start(model(),pos_integer()) -> ok.
start(Model, Time) ->
    start(Model, Time, []).


-spec start(model(),pos_integer(),[tuple()]) -> ok.
start(Model, Time, SimParamOptions) ->
    start(Model, Time, SimParamOptions, []).


-spec start(model(),pos_integer(),[tuple()],[tuple()]) -> ok.
start(Model, Time, SimParamOptions, ConfigOptions) ->
    SimParamsUpdated = misc_util:overwrite_options(SimParamOptions, load_params()),
    mas:start(?MODULE, Model, Time, proplist_to_record(SimParamsUpdated), ConfigOptions).


%% @doc function for starting `emas` from command line
-spec starts(list()) -> ok.
starts([Model, Time]) ->
    start(erlang:list_to_atom(Model), erlang:list_to_integer(Time)).


-spec initial_agent(sim_params()) -> agent().
initial_agent(SP) ->
    S = genetic:solution(SP),
    {S, genetic:evaluation(S, SP), SP#sim_params.initial_energy}.


%% @doc This function chooses a behaviour for the agent based on its energy.
-spec behaviour_function(agent(), sim_params()) -> agent_behaviour().
behaviour_function({_,_,0}, _SimParams) ->
    death;

behaviour_function({_, _, Energy}, SP) ->
    case random:uniform() < SP#sim_params.migration_probability of
        true -> migration;
        false -> case Energy > SP#sim_params.reproduction_threshold of
                     true -> reproduction;
                     false -> fight
                 end
    end.


-spec behaviours() -> [agent_behaviour()].
behaviours() ->
    [reproduction, death, fight, migration].


-spec meeting_function({agent_behaviour(), [agent()]}, sim_params()) -> [agent()].
meeting_function({death, _}, _SP) ->
    [];

meeting_function({reproduction, Agents}, SP) ->
    lists:flatmap(fun(Pair) ->
                          evolution:do_reproduce(Pair, SP)
                  end, evolution:optional_pairs(Agents, []));

meeting_function({fight, Agents}, SP) ->
    lists:flatmap(fun(Pair) ->
                          evolution:do_fight(Pair, SP)
                  end, evolution:optional_pairs(Agents, []));

meeting_function({migration, Agents}, _SP) ->
    Agents;

meeting_function({_, _}, _SP) ->
    erlang:error(unexpected_behaviour).


-spec stats() -> [funstat()].
stats() ->
    Fitness_map = fun({_Solution, Fitness, _Energy}) ->
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
    ConfigFile = filename:join(misc_util:get_config_dir(), "emas.config"),
    {ok, ParamsFromFile} = file:consult(ConfigFile),
    ParamsFromFile.

%% @doc Transform a proplist with simulation properties to a record
-spec proplist_to_record([tuple()]) -> sim_params().
proplist_to_record(Proplist) ->
    Dict = dict:from_list(Proplist),
    #sim_params{?LOAD(genetic_ops, Dict),
                ?LOAD(problem_size, Dict),
                ?LOAD(monitor_diversity, Dict),
                ?LOAD(initial_energy, Dict),
                ?LOAD(reproduction_threshold, Dict),
                ?LOAD(reproduction_transfer, Dict),
                ?LOAD(fight_transfer, Dict),
                ?LOAD(mutation_rate, Dict),
                ?LOAD(mutation_range, Dict),
                ?LOAD(mutation_chance, Dict),
                ?LOAD(migration_probability, Dict),
                ?LOAD(recombination_chance, Dict),
                ?LOAD(fight_number, Dict)}.
