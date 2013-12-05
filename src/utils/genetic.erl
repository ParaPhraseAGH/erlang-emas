%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul zawierajacy funkcje wykonujace operacje genetyczne

-module(genetic).
-export([solution/1, evaluation/1, reproduction/1, reproduction/2, generatePopulation/1, generateAgent/1]).

-type solution() :: [float()].
-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type island() :: [agent()].

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Funkcja generuje i zwraca losowego osobnika
-spec solution(integer()) -> solution().
solution(ProblemSize) ->
    [-50 + random:uniform() * 100 || _ <- lists:seq(1, ProblemSize)].

%% @doc Funkcja przyjmuje osobnika, oblicza i zwraca jego fitness.
-spec evaluation(solution()) -> float().
evaluation(S) ->
    - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end , 0.0, S).

%% @doc Funkcja generujaca losowego agenta
-spec generateAgent(pos_integer()) -> agent().
generateAgent(ProblemSize) ->
    S = solution(ProblemSize),
    {S, evaluation(S), config:initialEnergy()}.

%% @doc Funkcja generujaca losowa populacje.
-spec generatePopulation(pos_integer()) -> island().
generatePopulation(ProblemSize) ->
    [generateAgent(ProblemSize) || _ <- lists:seq(1, config:populationSize())].

%% @doc Funkcja reprodukcji dla pojedynczego osobnika (tylko mutacja).
-spec reproduction(solution()) -> solution().
reproduction(S) ->
    case random:uniform() < config:mutationChance() of
        true -> mutateSolution(S);
        false -> S
    end.
%% @doc Funkcja reprodukcji dla dwoch osobnikow (mutacja + krzyzowanie).
-spec reproduction(solution(),solution()) -> [solution()].
reproduction(S1, S2) ->
    {R1, R2} = case random:uniform() < config:recombinationChance() of
                   true -> recombineSolutions(S1, S2);
                   false -> {S1, S2}
               end,
    M1 = case random:uniform() < config:mutationChance() of
             true -> mutateSolution(R1);
             false -> R1
         end,
    M2 = case random:uniform() < config:mutationChance() of
             true -> mutateSolution(R2);
             false -> R2
         end,
    [M1, M2].

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc Funkcja krzyzujaca dwa osobniki
-spec recombineSolutions(solution(),solution()) -> {solution(),solution()}.
recombineSolutions(S1, S2) ->
    lists:unzip([recombineFeatures(F1, F2) || {F1, F2} <- lists:zip(S1,S2)]).

%% @doc Funkcja odpowiedzialna za skrzyzowanie dwoch pojedynczych genow (floatow).
-spec recombineFeatures(float(),float()) -> {float(),float()}.
recombineFeatures(F1, F2) ->
    A = erlang:min(F1, F2),
    B = (erlang:max(F1, F2) - erlang:min(F1, F2)),
    {A + random:uniform() * B,A + random:uniform() * B}.

%% @doc Funkcja mutujaca podanego osobnika
-spec mutateSolution(solution()) -> solution().
mutateSolution(S) ->
    NrGenesMutated = misc_util:averageNumber(config:mutationRate(),S),
    Indexes = [random:uniform(length(S)) || _ <- lists:seq(1,NrGenesMutated)], % indeksy moga sie powtarzac!
    mutateGenes(S,lists:usort(Indexes),1,[]). % usort usuwa powtorzenia

mutateGenes(RestOfSolution,[],_,Acc) ->
    lists:reverse(Acc,RestOfSolution);
mutateGenes([],[_|_],_,_) ->
    erlang:error(tooManyIndexes);
mutateGenes([Gene|Solution],[I|Indexes],I,Acc) ->
    mutateGenes(Solution,Indexes,I+1,[mutateFeature(Gene)|Acc]);
mutateGenes([Gene|Solution],[I|Indexes],Inc,Acc) ->
    mutateGenes(Solution,[I|Indexes],Inc+1,[Gene|Acc]).

%% @doc Funkcja mutujaca konkretny gen
-spec mutateFeature(float()) -> float().
mutateFeature(F) ->
    Range = config:mutationRange() * case random:uniform() of
                                         X when X < 0.2 -> 5.0;
                                         X when X < 0.4 -> 0.2;
                                         _ -> 1.0
                                     end,
    F + Range * math:tan(math:pi()*(random:uniform() - 0.5)).