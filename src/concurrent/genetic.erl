%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul zawierajacy funkcje wykonujace operacje genetyczne

-module(genetic).
-export([solution/0, evaluation/1, reproduction/1, reproduction/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec solution() -> List
%% @doc Funkcja generuje i zwraca losowego osobnika
solution() ->
  [-50 + random:uniform() * 100 || _ <- lists:seq(1, config:problemSize())].

%% @spec evaluation(List) -> float()
%% @doc Funkcja przyjmuje osobnika, oblicza i zwraca jego fitness.
evaluation(S) ->
    - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end , 0.0, S).

%% @spec reproduction(List1) -> List2
%% @doc Funkcja reprodukcji dla pojedynczego osobnika (tylko mutacja).
reproduction(S) ->
  case random:uniform() < config:mutationChance() of
    true -> mutateSolution(S);
    false -> S
  end.
%% @spec reproduction(List1,List2) -> List3
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

%% @spec recombineSolutions(List1,List2) -> {List3,List4}
%% @doc Funkcja krzyzujaca dwa osobniki
recombineSolutions(S1, S2) ->
  lists:unzip([ {recombineFeatures(F1, F2), recombineFeatures(F2, F1)} || {F1, F2} <- lists:zip(S1,S2)]).

%% @spec recombineFeatures(float(),float()) -> float()
%% @doc Funkcja odpowiedzialna za skrzyzowanie dwoch pojedynczych genow (floatow).
recombineFeatures(F1, F2) ->
  erlang:min(F1, F2) + random:uniform() * (erlang:max(F1, F2) - erlang:min(F1, F2)).

%% @spec mutateSolution(List1) -> List2
%% @doc Funkcja mutujaca podanego osobnika
mutateSolution(S) ->
  [ case random:uniform() < config:mutationRate() of
      true  -> mutateFeature(F);
      false -> F
    end
    || F <- S ].

%% @spec mutateFeature(float()) -> float()
%% @doc Funkcja mutujaca konkretny gen
mutateFeature(F) ->
  Range = config:mutationRange() * case random:uniform() of
                                     X when X < 0.2 -> 1.0 * 5;
                                     X when X < 0.4 -> 1.0 / 5;
                                     _ -> 1.0
                                   end,
  F + Range * math:tan(math:pi()*(random:uniform() - 0.5)).