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
-spec solution(integer()) -> solution().
%% @doc Funkcja generuje i zwraca losowego osobnika
solution(ProblemSize) ->
  [-50 + random:uniform() * 100 || _ <- lists:seq(1, ProblemSize)].

-spec evaluation(solution()) -> float().
%% @doc Funkcja przyjmuje osobnika, oblicza i zwraca jego fitness.
evaluation(S) ->
    - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end , 0.0, S).

-spec generateAgent(pos_integer()) -> agent().
%% @doc Funkcja generujaca losowego agenta
generateAgent(ProblemSize) ->
  S = solution(ProblemSize),
  {S, evaluation(S), config:initialEnergy()}.

-spec generatePopulation(pos_integer()) -> island().
%% @doc Funkcja generujaca losowa populacje.
generatePopulation(ProblemSize) ->
  [generateAgent(ProblemSize) || _ <- lists:seq(1, config:populationSize())].

-spec reproduction(solution()) -> solution().
%% @doc Funkcja reprodukcji dla pojedynczego osobnika (tylko mutacja).
reproduction(S) ->
  case random:uniform() < config:mutationChance() of
    true -> mutateSolution(S);
    false -> S
  end.
-spec reproduction(solution(),solution()) -> [solution()].
%% @doc Funkcja reprodukcji dla dwoch osobnikow (mutacja + krzyzowanie).
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
-spec recombineSolutions(solution(),solution()) -> {solution(),solution()}.
%% @doc Funkcja krzyzujaca dwa osobniki
recombineSolutions(S1, S2) ->
  lists:unzip([recombineFeatures(F1, F2) || {F1, F2} <- lists:zip(S1,S2)]).

-spec recombineFeatures(float(),float()) -> {float(),float()}.
%% @doc Funkcja odpowiedzialna za skrzyzowanie dwoch pojedynczych genow (floatow).
recombineFeatures(F1, F2) ->
  A = erlang:min(F1, F2),
  B = (erlang:max(F1, F2) - erlang:min(F1, F2)),
  {A + random:uniform() * B,A + random:uniform() * B}.

-spec mutateSolution(solution()) -> solution().
%% @doc Funkcja mutujaca podanego osobnika
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

-spec mutateFeature(float()) -> float().
%% @doc Funkcja mutujaca konkretny gen
mutateFeature(F) ->
  Range = config:mutationRange() * case random:uniform() of
                                     X when X < 0.2 -> 5.0;
                                     X when X < 0.4 -> 0.2;
                                     _ -> 1.0
                                   end,
  F + Range * math:tan(math:pi()*(random:uniform() - 0.5)).