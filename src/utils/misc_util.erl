%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami pomocniczymi dla roznych wersji algorytmu.

-module(misc_util).
-export([groupBy/1, shuffle/1, behavior/1, behavior_noMig/1, clearInbox/0, result/1, find/2, averageNumber/2, mapIndex/4,
         seedRandom/0, countGroups/2, addCounters/2, hybridDiversity/1]).

-record(counter,{fight = 0 :: non_neg_integer(),
                 reproduction = 0 :: non_neg_integer(),
                 migration = 0 :: non_neg_integer(),
                 death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type task() :: death | fight | reproduction | migration.
-type groups() :: [{task(),[agent()]}].
-type counter() :: #counter{}.

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


-spec hybridDiversity([agent()]) -> {[float()], [float()]}.
%% @doc Funkcja wyliczajaca sume i minimum odchylen standardowych genotypow agentow
hybridDiversity(Agents) ->
    Solutions = [Sol || {Sol, _, _} <- Agents],
    Variances = [stddev(Sol) || Sol <- transpose(Solutions)],
    Sum = lists:sum(Variances),
    Min = lists:min(Variances),
    {Sum, Min}.


-spec countGroups([tuple()],counter()) -> counter().
%% @doc Zwraca liczby agentow nalezacych do poszczegolnych kategorii w formie rekordu
countGroups([],Counter) ->
    Counter;
countGroups([{death,AgentList}|Groups],Counter) ->
    countGroups(Groups,Counter#counter{death = length(AgentList)});
countGroups([{migration,AgentList}|Groups],Counter) ->
    countGroups(Groups,Counter#counter{migration = length(AgentList)});
countGroups([{fight,AgentList}|Groups],Counter) ->
    countGroups(Groups,Counter#counter{fight = length(AgentList)});
countGroups([{reproduction,AgentList}|Groups],Counter) ->
    countGroups(Groups,Counter#counter{reproduction = length(AgentList)}).

-spec addCounters(counter(),counter()) -> counter().
%% @doc Funkcja dodaje dwa liczniki
addCounters(C1,C2) ->
    #counter{fight = C1#counter.fight + C2#counter.fight,
             reproduction = C1#counter.reproduction + C2#counter.reproduction,
             death = C1#counter.death + C2#counter.death,
             migration = C1#counter.migration + C2#counter.migration}.

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

-spec stddev([float()]) -> float().
%% @doc Funkcja wyliczajaca odchylenie standardowe rozkladu zadanego przez liste liczb
stddev(L) ->
  {SqSum, Sum, Len} = lists:foldl(fun (X, {K,S,N}) ->
    {K+X*X,S+X,N+1}
  end, {0,0,0}, L),
  Mean = Sum/Len,
  math:sqrt(SqSum/Len-Mean*Mean).

-spec transpose([[float()]]) -> [[float()]].
%% @doc Funkcja wykonujaca operacje zip na dowolnej liczbie list wejsciowych
%% http://erlang.org/pipermail/erlang-questions/2012-October/069856.html
transpose([[X | Xs] | Xss]) ->
  [[X | [H || [H | _] <- Xss]] | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [];
transpose(Tuple) when is_tuple(Tuple) ->  % wrapper to emulate zip
  Xs = transpose(tuple_to_list(Tuple)),
  [list_to_tuple(X) || X <- Xs].

