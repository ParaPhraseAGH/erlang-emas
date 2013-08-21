%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul zawierajacy funkcje pomocnicze dla innych modulow.

-module(emas_util).
-export([result/1, groupBy/2, behavior/1, shuffle/1, optionalPairs/1, print/1, multiAppend/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec groupBy(function(),List1) -> List2
%% @doc Funkcja grupujaca agentow do krotek przy pomocy funkcji F.
%% Zwracana jest lista w formie [{migration,[A1,A2]},{fight,[A3,A4,A5]}]
groupBy(F, L) ->
  dict:to_list(
    lists:foldr(fun({K,V}, D) ->
      dict:append(K, V, D)
    end , dict:new(), [ {F(X), X} || X <- L ])).

%% @spec shuffle(List1) -> List2
%% @doc Funkcja mieszajaca podana liste.
shuffle(L) ->
  Rand = [{random:uniform(), N} || N <- L],
  [X||{_,X} <- lists:sort(Rand)].

%% @spec behavior(Agent) -> death | reproduction | fight
%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie
%% jego energii.
behavior({_,_,0}) ->
  death;
behavior({_, _, Energy}) ->
  case Energy > config:reproductionThreshold() of
    true -> reproduction;
    false -> fight
  end.

%% @spec multiAppend(int(),List1,List2) -> List3
%% @doc Funkcja dorzucajaca N agentow z List1 do kazdej z wysp w List2.
%% Zwracana jest lista wysp z dodanymi agentami.
multiAppend(_,[],_) -> [];
multiAppend(N,Shuffled,[I|Rest]) ->
  {Imigrants,Tail} = lists:split(N,Shuffled),
  [lists:append(I,Imigrants) | multiAppend(N,Tail,Rest)].

%% @spec optionalPairs(List1) -> List2
%% @doc Funkcja dzielaca podana liste agentow na pary. Przykladowe dzialanie:
%% [A1,A2,A3,A4,A5] -> [{A1,A2},{A3,A4},{A5}]
optionalPairs(L) ->
  optionalPairsTail(L,[]).

%% @spec print(float()) -> ok
%% @doc Funkcja wypisujaca fitness na ekran.
print(Fitness) ->
  io:format("Fitness: ~p~n",[Fitness]).

%% @spec result(List1) -> float()
%% @doc Funkcja okreslajaca najlepszy wynik na podstawie przeslanej listy agentow
result(Agents) ->
  lists:max([ Fitness || {_ ,Fitness, _} <- Agents]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec optionalPairsTail(List1,List2) -> List3
%% @doc Funkcja dzielaca podana liste agentow na pary. Tail recursion.
optionalPairsTail([],Acc) -> Acc;
optionalPairsTail([A],Acc) -> [{A}|Acc];
optionalPairsTail([A,B|L],Acc) -> optionalPairsTail(L,[{A,B}|Acc]).