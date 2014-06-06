-module (rastrigin_nif_ops).
-behaviour (genetic_ops).
-export ([solution/1, evaluation/1,
    mutation/1, mutation/3, recombination/2]).
-on_load(init/0).
-define (LIBNAME, "rastrigin_nif").
-include ("emas.hrl").

init() ->
    SOName = filename:join([priv, ?LIBNAME]) , 
    ok = erlang:load_nif(SOName, 0).

%% @doc Funkcja generuje i zwraca losowego osobnika
-spec solution(integer()) -> solution().
solution(_ProblemSize) ->
    exit(nif_not_loaded).

%% @doc Funkcja przyjmuje osobnika, oblicza i zwraca jego fitness.
-spec evaluation(solution()) -> float().
evaluation(_Solution) ->
    exit(nif_not_loaded).

%% @doc Funkcja mutujaca podanego osobnika
-spec mutation(solution()) -> solution().
mutation(_Solution, _Range, _Rate) ->
    exit(nif_not_loaded).

%% @doc Funkcja krzyzujaca dwa osobniki
-spec recombination(solution(),solution()) -> {solution(),solution()}.
recombination(_Solution1, _Solution2) ->
    exit(nif_not_loaded).

mutation(_Solution) ->
    mutation(_Solution, emas_config:mutationRange(), emas_config:mutationRate()).