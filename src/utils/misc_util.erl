%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami pomocniczymi dla roznych wersji algorytmu.

-module(misc_util).
-export([groupBy/1, shuffle/1, behavior/1, behavior_noMig/1, clearInbox/0, result/1, find/2, averageNumber/2, mapIndex/4,
        seedRandom/0]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type task() :: death | fight | reproduction | migration.
-type groups() :: [{task(),[agent()]}].

%% ====================================================================
%% API functions
%% ====================================================================
-spec groupBy([{term(),term()}]) -> groups().
%% @doc Funkcja grupujaca krotki wg schematu:
%% [{K1,V1},{K2,V2},...] -> [{K1,[V1,V3]},{K2,[V2,V4,V5]},...]
groupBy(List) ->
  dict:to_list(
    lists:foldl(fun({K,V}, D) ->
      dict:append(K, V, D)
    end , dict:new(), List)).

-spec shuffle(list()) -> list().
%% @doc Funkcja mieszajaca podana liste.
shuffle(L) ->
  Rand = [{random:uniform(), N} || N <- L],
  [X||{_,X} <- lists:sort(Rand)].

-spec behavior(agent() | {agent(),pos_integer()}) -> task().
%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie jego energii.
behavior({_,_,0}) ->
  death;
behavior({_, _, Energy}) ->
  case random:uniform() < config:migrationProbability() of
    true -> migration;
    false -> case Energy > config:reproductionThreshold() of
               true -> reproduction;
               false -> fight
             end
  end;
behavior({_Island,Agent}) when is_tuple(Agent) ->
  behavior(Agent).

-spec behavior_noMig(agent()) -> death | reproduction | fight.
%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie jego energii.
behavior_noMig({_,_,0}) ->
  death;
behavior_noMig({_, _, Energy}) ->
  case Energy > config:reproductionThreshold() of
    true -> reproduction;
    false -> fight
  end.

-spec averageNumber(float(),[term()]) -> integer().
%% @doc Funkcja wyznacza statystyczna liczbe elementow, ktore podlegaja jakiejs operacji z danym prawdopodobienstwem
averageNumber(Probability,List) ->
  N = Probability * length(List),
  if N == 0 -> 0;
    N < 1 ->
      case random:uniform() < N of
        true -> 1;
        false -> 0
      end;
    N >=1 -> trunc(N)
  end.

-spec mapIndex(Elem::term(), Index::integer(), List::[term()], F::fun()) -> [term()].
%% @doc Funkcja wykonuje funkcje F na elemencie listy List o indeksie Index oraz parametrze Elem.
%% Wynik tej funkcji jest podmieniany jako nowy element o tym indeksie.
mapIndex(Elem,Index,List,F) ->
  mapIndex(Elem,Index,List,F,[]).

-spec clearInbox() -> ok.
%% @doc Funkcja czyszczaca skrzynke.
clearInbox() ->
  receive
    _ -> clearInbox()
  after 0 ->
    ok
  end.

-spec find(term(),[term()]) -> integer().
%% @doc Funkcja wyznaczajaca indeks pod jakim znajduje sie dany element na podanej liscie.
find(Elem,List) ->
  find(Elem,List,1).

-spec result([agent()]) -> float() | islandEmpty.
%% @doc Funkcja okreslajaca najlepszy wynik na podstawie przeslanej listy agentow
result(Agents) ->
  case Agents of
    [] ->
      islandEmpty;
    _ ->
      lists:max([ Fitness || {_ ,Fitness, _} <- Agents])
  end.

-spec seedRandom() -> {integer(),integer(),integer()}.
seedRandom() ->
  {_,B,C} = erlang:now(),
  List = atom_to_list(node()),
  Hash = lists:foldl(fun(N,Acc) ->
                       if N >= 1000 -> Acc * 10000 + N;
                         N >= 100 -> Acc * 1000 + N;
                         N >= 10 -> Acc * 100 + N;
                         N < 10 -> Acc * 10 + N
                       end
                     end,0,List),
  random:seed(Hash,B,C).

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec find(term(),[term()],integer()) -> integer().
%% @doc Funkcja wyznaczajaca indeks pod jakim znajduje sie dany element na podanej liscie.
find(Elem,[Elem|_],Inc) ->
  Inc;
find(_,[],_) ->
  notFound;
find(Elem,[_|T],Inc) ->
  find(Elem,T,Inc+1).

-spec mapIndex(Elem::term(), Index::integer(), List::[term()], F::fun(), Acc::[term()]) -> [term()].
%% @doc Funkcja wykonuje funkcje F na elemencie listy List o indeksie Index oraz parametrze Elem.
%% Wynik tej funkcji jest podmieniany jako nowy element o tym indeksie.
mapIndex(_,_,[],_,_) ->
  erlang:error(wrongIndex);
mapIndex(Elem,1,[H|T],F,Acc) ->
  lists:reverse(Acc,[F(Elem,H)|T]);
mapIndex(Elem,Index,[H|T],F,Acc) ->
  mapIndex(Elem,Index - 1,T,F,[H|Acc]).

