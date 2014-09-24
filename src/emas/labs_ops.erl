-module (labs_ops).
-behaviour (genetic_ops).
-export ([solution/1, evaluation/1, mutation/1, recombination/2]).

-include ("emas.hrl").

%% @doc Generates a random solution.
-spec solution(integer()) -> solution().
solution(ProblemSize) ->
   [random:uniform(2)-1 || _ <- lists:seq(1, ProblemSize)].


%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution()) -> float().
evaluation(Solution) ->
    L = length(Solution),
    Cs = [foldzip(drop(Solution, K), Solution)
        || K <- lists:seq(1, L-1)],
    E = lists:foldl(fun (X, Acc) -> X*X + Acc end, 0, Cs),
    L*L*0.5/E.


-spec recombination(solution(),solution()) -> {solution(),solution()}.
recombination(S1, S2) ->
    lists:unzip([recombinationFeatures(F1, F2) || {F1, F2} <- lists:zip(S1,S2)]).

%% @doc Chooses a random order between the two initial features.
-spec recombinationFeatures(float(),float()) -> {float(),float()}.
recombinationFeatures(F, F) -> {F, F};
recombinationFeatures(F1, F2) ->
    case random:uniform() < 0.5 of
        true -> {F1, F2};
        false -> {F2, F1}
    end.

%% @doc Reproduction function for a single agent (mutation only).
-spec mutation(solution()) -> solution().
mutation(Solution) ->
    lists:map(fun(X) -> 
        case random:uniform() < emas_config:mutationRate() of
            true -> -X + 1;
            _ -> X
        end
    end, Solution).

%% internal functions

drop([], _) -> [];
drop(L, 0) -> L;
drop([_ | T], N) ->
    drop(T, N - 1).

foldzip(A, B) -> foldzip(A, B, 0).

foldzip([], _, Acc) -> Acc;
foldzip(_, [], Acc) -> Acc;
foldzip([HA|TA], [HB|TB], Acc) ->
    foldzip(TA, TB, Acc + dot(HA,HB)).

dot(N,N) -> 1;
dot(_,_) -> -1.

