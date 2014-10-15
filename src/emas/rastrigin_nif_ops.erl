-module (rastrigin_nif_ops).
-behaviour (emas_genetic_ops).
-export ([solution/1, evaluation/2,
    mutation/2, mutation/3, recombination/3]).
-on_load(init/0).
-define (LIBNAME, "rastrigin_nif").
-include ("emas.hrl").

init() ->
    SOName = filename:join([priv, ?LIBNAME]) , 
    ok = erlang:load_nif(SOName, 0).

%% @doc Generates a random solution.
-spec solution(sim_params()) -> solution().
solution(_SP) ->
    exit(nif_not_loaded).

%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution(), sim_params()) -> float().
evaluation(_Solution, _SP) ->
    exit(nif_not_loaded).

%% @doc Reproduction function for a single agent (mutation only).
-spec mutation(solution(), float(), float()) -> solution().
mutation(_Solution, _Range, _Rate) ->
    exit(nif_not_loaded).

%% @doc FReproduction function for a pair of agents (crossover and mutation).
-spec recombination(solution(), solution(), sim_params()) -> {solution(), solution()}.
recombination(_Solution1, _Solution2, _SP) ->
    exit(nif_not_loaded).


-spec mutation(solution(), sim_params()) -> solution().
mutation(_Solution, SP) ->
    mutation(_Solution, SP#sim_params.mutation_range, SP#sim_params.mutation_rate).