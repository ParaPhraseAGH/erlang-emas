-module(rastrigin_bin_ops).

-behaviour(genetic_ops).

-export ([evaluation/1, mutation/1, recombination/2, solution/1]).

-include ("emas.hrl").

%% @doc Generates a random solution, as a vector of numbers in the range [-50, 50].
-spec solution(integer()) -> solution().
solution(ProblemSize) ->
    S = [-50 + random:uniform() * 100 || _ <- lists:seq(1, ProblemSize)],
    erlang:term_to_binary(S).


%% @doc Evaluates a given solution by computing the Rastrigin function.
-spec evaluation(solution()) -> float().
evaluation(B) ->
    S = erlang:binary_to_term(B),
    - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end , 0.0, S).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Continuously recombines every pair of features for the given pair of solutions.
-spec recombination(solution(),solution()) -> {solution(),solution()}.
recombination(B1, B2) ->
    S1 = erlang:binary_to_term(B1),
    S2 = erlang:binary_to_term(B2),
    {S3, S4} = lists:unzip([recombinationFeatures(F1, F2) || {F1, F2} <- lists:zip(S1,S2)]),
    {erlang:term_to_binary(S3), erlang:term_to_binary(S4)}.

%% @doc Chooses a random value between the two initial features.
-spec recombinationFeatures(float(),float()) -> {float(),float()}.
recombinationFeatures(F1, F2) ->
    A = erlang:min(F1, F2),
    B = (erlang:max(F1, F2) - erlang:min(F1, F2)),
    {A + random:uniform() * B,A + random:uniform() * B}.

%% @doc Mutates the features at random indices
-spec mutation(solution()) -> solution().
mutation(B) ->
    S = erlang:binary_to_term(B),
    NrGenesMutated = misc_util:averageNumber(emas_config:mutationRate(),S),
    Indexes = [random:uniform(length(S)) || _ <- lists:seq(1,NrGenesMutated)], % indices may be duplicated
    Mut = mutateGenes(S,lists:usort(Indexes),1,[]), % usort removes duplicates
    erlang:term_to_binary(Mut).

mutateGenes(RestOfSolution,[],_,Acc) ->
    lists:reverse(Acc,RestOfSolution);
mutateGenes([],[_|_],_,_) ->
    erlang:error(tooManyIndexes);
mutateGenes([Gene|Solution],[I|Indexes],I,Acc) ->
    mutateGenes(Solution,Indexes,I+1,[mutateFeature(Gene)|Acc]);
mutateGenes([Gene|Solution],[I|Indexes],Inc,Acc) ->
    mutateGenes(Solution,[I|Indexes],Inc+1,[Gene|Acc]).

%% @doc Actually mutates a given feature.
-spec mutateFeature(float()) -> float().
mutateFeature(F) ->
    Range = emas_config:mutationRange() * case random:uniform() of
                                         X when X < 0.2 -> 5.0;
                                         X when X < 0.4 -> 0.2;
                                         _ -> 1.0
                                     end,
    F + Range * math:tan(math:pi()*(random:uniform() - 0.5)).