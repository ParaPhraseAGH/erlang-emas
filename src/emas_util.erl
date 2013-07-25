%% @author krzywick
%% @doc @todo Add description to emas_util.


-module(emas_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([groupBy/2, shuffle/1, optionalPairs/1]).

groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ]).

shuffle([]) -> [];
shuffle([Elem]) -> [Elem];
shuffle(List) -> shuffle(List, []).

optionalPairs([]) -> [];
optionalPairs([A]) -> [{A}];
optionalPairs([A,B|L]) -> [ {A,B} | optionalPairs(L)].


%% ====================================================================
%% Internal functions
%% ====================================================================

shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).

