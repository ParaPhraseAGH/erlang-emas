%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami pomocniczymi dla roznych wersji algorytmu.

-module(misc_util).
-export([groupBy/1, shuffle/1, behavior/1, behavior_noMig/1, clearInbox/0, result/1, index/2]).

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

-spec clearInbox() -> ok.
%% @doc Funkcja czyszczaca skrzynke.
clearInbox() ->
  receive
    _ -> clearInbox()
  after 0 ->
    ok
  end.

-spec index(term(),[term()]) -> integer().
%% @doc Funkcja wyznaczajaca indeks pod jakim znajduje sie dany element na podanej liscie.
index(Elem,List) ->
  index(Elem,List,1).

-spec result([agent()]) -> float() | islandEmpty.
%% @doc Funkcja okreslajaca najlepszy wynik na podstawie przeslanej listy agentow
result(Agents) ->
  case Agents of
    [] ->
      islandEmpty;
    _ ->
      lists:max([ Fitness || {_ ,Fitness, _} <- Agents])
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec index(term(),[term()],integer()) -> integer().
%% @doc Funkcja wyznaczajaca indeks pod jakim znajduje sie dany element na podanej liscie.
index(Elem,[Elem|_],Inc) ->
  Inc;
index(_,[],_) ->
  notFound;
index(Elem,[_|T],Inc) ->
  index(Elem,T,Inc+1).

