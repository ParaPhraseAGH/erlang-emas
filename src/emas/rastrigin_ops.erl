-module(rastrigin_ops).

-behaviour(genetic_ops).

-export ([evaluation/1, mutation/2, recombination/2, solution/1]).

-include ("emas.hrl").

%% @doc Generates a random solution, as a vector of numbers in the range [-50, 50].
-spec solution(integer()) -> solution().
solution(ProblemSize) ->
    [-50 + random:uniform() * 100 || _ <- lists:seq(1, ProblemSize)].


%% @doc Evaluates a given solution by computing the Rastrigin function.
-spec evaluation(solution()) -> float().
evaluation(S) ->
    - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end , 0.0, S).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Continuously recombines every pair of features for the given pair of solutions.
-spec recombination(solution(),solution()) -> {solution(),solution()}.
recombination(S1, S2) ->
    lists:unzip([recombination_features(F1, F2) || {F1, F2} <- lists:zip(S1,S2)]).

%% @doc Chooses a random value between the two initial features.
-spec recombination_features(float(),float()) -> {float(),float()}.
recombination_features(F1, F2) ->
    A = erlang:min(F1, F2),
    B = (erlang:max(F1, F2) - erlang:min(F1, F2)),
    {A + random:uniform() * B,A + random:uniform() * B}.

%% @doc Mutates the features at random indices
-spec mutation(solution(), sim_params()) -> solution().
mutation(S, SimParams) ->
    NrGenesMutated = misc_util:averageNumber(SimParams#sim_params.mutation_rate, S),
    Indexes = [random:uniform(length(S)) || _ <- lists:seq(1, NrGenesMutated)], % indices may be duplicated
    mutate_genes(S, lists:usort(Indexes), 1, [], SimParams). % usort removes duplicates

mutate_genes(RestOfSolution, [], _, Acc, _SimParams) ->
    lists:reverse(Acc,RestOfSolution);
mutate_genes([], [_|_], _, _, _) ->
    erlang:error(tooManyIndexes);
mutate_genes([Gene|Solution], [I|Indexes], I, Acc, SimParams) ->
    mutate_genes(Solution, Indexes, I+1, [mutate_feature(Gene, SimParams)|Acc], SimParams);
mutate_genes([Gene|Solution], [I|Indexes], Inc, Acc, SimParams) ->
    mutate_genes(Solution, [I|Indexes], Inc+1, [Gene|Acc], SimParams).

%% @doc Actually mutates a given feature.
-spec mutate_feature(float(), sim_params()) -> float().
mutate_feature(F, SimParams) ->
    Range = SimParams#sim_params.mutation_range * case random:uniform() of
                                         X when X < 0.2 -> 5.0;
                                         X when X < 0.4 -> 0.2;
                                         _ -> 1.0
                                     end,
    F + Range * math:tan(math:pi()*(random:uniform() - 0.5)).