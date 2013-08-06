%% @author krzywick
%% @doc @todo Add description to genetic.


-module(genetic).

%% ====================================================================
%% API functions
%% ====================================================================
-export([solution/0, evaluation/1, reproduction/1, reproduction/2]).


solution() -> [-50 + random:uniform() * 100 || _ <- lists:seq(1, config:problemSize())].
evaluation(S) -> - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end , 0.0, S).

reproduction(S) -> 
	case random:uniform() < config:mutationChance() of
		true -> mutateSolution(S);
		false -> S
	end.
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
%%TODO recombine features szybciej
recombineSolutions(S1, S2) -> lists:unzip([ {recombineFeatures(F1, F2), recombineFeatures(F2, F1)} || {F1, F2} <- lists:zip(S1,S2)]).

recombineFeatures(F1, F2) -> erlang:min(F1, F2) + random:uniform() * (erlang:max(F1, F2) - erlang:min(F1, F2)).

mutateSolution(S) -> 
	[ case random:uniform() < config:mutationRate() of
		true  -> mutateFeature(F); 
		false -> F
	  end 
	|| F <- S ].

mutateFeature(F) -> 
	Range = config:mutationRange() * case random:uniform() of
				X when X < 0.2 -> 1.0 * 5;
				X when X < 0.4 -> 1.0 / 5;
				_ -> 1.0
			end,
	F + Range * math:tan(math:pi()*(random:uniform() - 0.5)).