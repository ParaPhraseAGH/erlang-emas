%% @author krzywick
%% @doc @todo Add description to genetic.


-module(genetic).

%% ====================================================================
%% API functions
%% ====================================================================
-export([solution/0, evaluation/1, reproduction/1, reproduction/2]).


solution() -> random:uniform().
evaluation(S) -> S.

reproduction(S) -> mutate(S). 
reproduction(S1, S2) -> 
	Avg = (S1 + S2)/2,
	[mutate(Avg), mutate(Avg)].



%% ====================================================================
%% Internal functions
%% ====================================================================

limit(S) -> erlang:max(0.0, erlang:min(1.0, S)).
mutate(S) -> limit(S + random:uniform()*0.002 - 0.001).