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
    local_search(Solution).

-spec energy(solution()) -> float().
energy(Solution) ->
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
            true -> fnot(X);
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

dot(X, X) -> 1;
dot(_, _) -> -1.

fnot(X) -> -X + 1.

-spec local_search(solution()) -> float().
local_search(Solution) ->
    MaxIterations = 25,
    {_Sol, Eval} = local_search(MaxIterations, Solution, energy(Solution)),
    Eval.

-spec local_search(integer(), solution(), float()) -> {solution(), float()}.
local_search(0, Solution, Evaluation) ->
    {Solution, Evaluation};
local_search(RemainingSteps, Solution, Evaluation) ->
    {BestSol, BestEval} = best_flipped(Solution),
    case BestEval > Evaluation of
        true -> local_search(RemainingSteps-1, BestSol, BestEval);
        _ -> {Solution, Evaluation}
    end.

best_flipped(Solution) ->
    FlippedSols = lists:map(fun (I) -> flip_nth(Solution,I) end,
              lists:seq(1, length(Solution))),
    First = hd(FlippedSols),
    InitAcc = {First, energy(First)},
    GetBest = fun (S, {AccSol, AccE}) ->
        E = energy(S),
        case E > AccE of
            true -> {S, E};
            _ -> {AccSol, AccE}
        end
    end,
    lists:foldl(GetBest, InitAcc, FlippedSols).

flip_nth(Sol, N) ->
    flip_nth(Sol, [], N).

flip_nth([HS | TS], Acc, 1) ->
    lists:reverse(Acc) ++ [fnot(HS) | TS];
flip_nth([HS | TS], Acc, N) ->
    flip_nth(TS, [HS | Acc], N-1).
