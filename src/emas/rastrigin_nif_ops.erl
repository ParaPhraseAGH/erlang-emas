-module (rastrigin_nif_ops).
-behaviour (genetic_ops).
-export ([solution/1, evaluation/1,
    mutation/2, mutation/3, recombination/2]).
-on_load(init/0).
-define (LIBNAME, "rastrigin_nif").
-include ("emas.hrl").

init() ->
    SOName = filename:join([priv, ?LIBNAME]) , 
    ok = erlang:load_nif(SOName, 0).

%% @doc Generates a random solution.
-spec solution(integer()) -> solution().
solution(_ProblemSize) ->
    exit(nif_not_loaded).

%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution()) -> float().
evaluation(_Solution) ->
    exit(nif_not_loaded).

%% @doc Reproduction function for a single agent (mutation only).
-spec mutation(solution(), float(), float()) -> solution().
mutation(_Solution, _Range, _Rate) ->
    exit(nif_not_loaded).

%% @doc FReproduction function for a pair of agents (crossover and mutation).
-spec recombination(solution(),solution()) -> {solution(),solution()}.
recombination(_Solution1, _Solution2) ->
    exit(nif_not_loaded).

mutation(_Solution, SimParams) ->
    mutation(_Solution, SimParams#sim_params.mutation_range, SimParams#sim_params.mutation_rate).