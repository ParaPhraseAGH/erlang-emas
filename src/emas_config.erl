-module(emas_config).
-include("emas.hrl").

-export([proplist_to_record/1,
         options_specs/0 ]).



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
proplist_to_record(Options) ->
    Proplist = Options ++ default_options(),
    #sim_params{?LOAD(Proplist, genetic_ops),
                ?LOAD(Proplist, problem_size),
                ?LOAD(Proplist, initial_energy),
                ?LOAD(Proplist, reproduction_threshold),
                ?LOAD(Proplist, reproduction_transfer),
                ?LOAD(Proplist, fight_transfer),
                ?LOAD(Proplist, mutation_rate),
                ?LOAD(Proplist, mutation_range),
                ?LOAD(Proplist, mutation_chance),
                ?LOAD(Proplist, recombination_chance),
                ?LOAD(Proplist, fight_number)}.



-spec options_specs() -> [getopt:option_spec()].
options_specs() ->
    [{genetic_ops,            undefined, "genetic_ops",           {atom, emas_test_ops},
      "the name of the module with the genetic operators and the genetic callback implementation"},

     {problem_size,           undefined, "problem_size",          {integer, 100},
      "The size of the problem represented by an agent"},

     {initial_energy,         undefined, "initial_energy",        {integer, 10},
      "Agent initial energy"},

     {reproduction_threshold, undefined, "rep_threshold",         {integer, 11},
      "Amount of energy above which agents reproduce"},

     {reproduction_transfer,  undefined, "rep_transfer",          {integer, 5},
     "Maximal amount of energy given by a parent to its child during reproduction"},

     {fight_transfer,         undefined, "fight_transfer",        {integer, 10},
      "Maximal amount of energy given by the loser to a winner during the fight"},

     {mutation_rate,          undefined, "mutation_rate",         {float, 0.1},
      "How much of a solution should mutate <0.0 ... 1.0>"},

     {mutation_range,         undefined, "mutation_range",        {float, 0.05},
      "How big is the mutation <0.0 .. 1.0>"},

     {mutation_chance,        undefined, "mutation_chance",       {float, 0.75},
      "The probability to mutate a solution during reproduction <0.0 .. 1.0>"},

     {recombination_chance,   undefined, "recombination_chance",  {float, 0.3},
      "The probability to recombine parent solutions during reproduction <0.0 .. 1.0>"},

     {fight_number,           undefined, "fight_number",          {integer, 2},
      "The size of the fight arena"}].


default_options() ->
    {ok,{Options, [] = _NotParsed}}
        = getopt:parse(options_specs(), ""),
    Options.
