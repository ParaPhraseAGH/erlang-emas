-module(emas_config).
-include("emas.hrl").

-export([proplist_to_record/1]).



-define(LOAD(Proplist, Prop),
        Prop = case proplists:lookup(Prop, Proplist) of
                   {Prop, Value} ->
                       Value;
                   none ->
                       erlang:error({"mas missing option", Prop})
               end).

-define(LOAD(Proplist, Prop, Default),
        Prop = proplists:get_value(Prop, Proplist, Default)).


%% @doc Transform a proplist with simulation properties to a record
-spec proplist_to_record([{atom(), term()}]) -> mas:sim_params().
proplist_to_record(Proplist) ->
    #sim_params{?LOAD(Proplist, genetic_ops, emas_test_ops),
                ?LOAD(Proplist, problem_size, 100),
                ?LOAD(Proplist, initial_energy, 10),
                ?LOAD(Proplist, reproduction_threshold, 11),
                ?LOAD(Proplist, reproduction_transfer, 5),
                ?LOAD(Proplist, fight_transfer, 10),
                ?LOAD(Proplist, mutation_rate, 0.1),
                ?LOAD(Proplist, mutation_range, 0.05),
                ?LOAD(Proplist, mutation_chance, 0.75),
                ?LOAD(Proplist, recombination_chance, 0.3),
                ?LOAD(Proplist, fight_number, 2)}.
