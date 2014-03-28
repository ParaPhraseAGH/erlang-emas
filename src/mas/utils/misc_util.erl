%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami pomocniczymi dla roznych wersji algorytmu.

-module(misc_util).
-export([groupBy/1, shuffle/1, clearInbox/0, result/1, find/2, averageNumber/2, mapIndex/4, shortestZip/2,
         seedRandom/0, countGroups/2, addCounters/2, logNow/1, diversity/1, onlineDiversity/5, meeting_proxy/2]).

-record(counter,{fight = 0 :: non_neg_integer(),
                 reproduction = 0 :: non_neg_integer(),
                 migration = 0 :: non_neg_integer(),
                 death = 0 :: non_neg_integer()}).

-type counter() :: #counter{}.
-type model() :: sequential | hybrid | concurrent.

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Funkcja grupujaca krotki wg schematu:
%% [{K1,V1},{K2,V2},...] -> [{K1,[V1,V3]},{K2,[V2,V4,V5]},...]
-spec groupBy([{term(),term()}]) -> [{term(),[term()]}].
groupBy(List) ->
    dict:to_list(
      lists:foldl(fun({K,V}, D) ->
                          dict:append(K, V, D)
                  end , dict:new(), List)).

%% @doc Funkcja mieszajaca podana liste.
-spec shuffle(list()) -> list().
shuffle(L) ->
    Rand = [{random:uniform(), N} || N <- L],
    [X||{_,X} <- lists:sort(Rand)].

-spec meeting_proxy({atom(),list()},model()) -> list().
meeting_proxy({migration,_Agents},sequential) ->
    [];

meeting_proxy({migration,Agents},hybrid) ->
    [hybrid:sendAgent(Agent) || Agent <- Agents],
    [];

meeting_proxy({migration,_Agents},concurrent) ->
    [];

meeting_proxy(Group,_) ->
    Environment = config:agent_env(),
    Environment:meeting_function(Group).

%% @doc Funkcja wyznacza statystyczna liczbe elementow, ktore podlegaja jakiejs operacji z danym prawdopodobienstwem
-spec averageNumber(float(),[term()]) -> integer().
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

-spec logNow(erlang:timestamp()) -> {yes,erlang:timestamp()} | notyet.
logNow(LastLog) ->
    Now = os:timestamp(),
    Diff = timer:now_diff(Now,LastLog),
    IntervalInMicros = config:writeInterval()*1000,
    if
        Diff >= IntervalInMicros ->
            {Mega,Sec,Micro} = LastLog,
            {yes,{Mega,Sec+1,Micro}};
        true ->
            notyet
    end.

%% @doc Funkcja wyliczajaca sume i minimum odchylen standardowych genotypow agentow
-spec diversity([term()]) -> {float(), float(), float()}.
diversity([]) ->
    {-1.0, -1.0, 0.0};
diversity(Agents) ->
    Solutions = [Sol || {Sol, _, _} <- Agents],
    Variances = [variance(Sol) || Sol <- transpose(Solutions)],
    Sum = lists:sum(Variances),
    Min = lists:min(Variances),
    Var = variance(Variances),
    {Sum, Min, Var}.

%% @doc Funkcja online wyliczajaca sume i minimum odchylen standardowych genotypow agentow
-spec onlineDiversity(genetic:solution(), atom(), integer(), [float()],[float()]) -> {float(),float(),float(), [float()], [float()]}.
onlineDiversity(_Solution, _Fun, N, PrevMeans, PrevStds) when N < 1 ->
    %%     io:format("Diversity incalculable~n"),
    {-1.0,-1.0,-1.0,PrevMeans,PrevStds};

onlineDiversity(Solution, F, N, PrevMeans, PrevMs) ->
    {NewMeans, Ms} = lists:unzip([onlineCount(T, N, F) || T <- lists:zip3(Solution, PrevMeans, PrevMs)]),
    Stds = [X/N || X <- Ms],
    Sum = lists:sum(Stds),
    Min = lists:min(Stds),
    Var = variance(Stds),
    {Sum, Min, Var, NewMeans, Ms}.

%% @doc Zwraca liczby agentow nalezacych do poszczegolnych kategorii w formie rekordu
-spec countGroups([tuple()],counter()) -> counter().
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

%% @doc Funkcja dodaje dwa liczniki
-spec addCounters(counter(),counter()) -> counter().
addCounters(C1,C2) ->
    #counter{fight = C1#counter.fight + C2#counter.fight,
             reproduction = C1#counter.reproduction + C2#counter.reproduction,
             death = C1#counter.death + C2#counter.death,
             migration = C1#counter.migration + C2#counter.migration}.

%% @doc Funkcja wykonuje funkcje F na elemencie listy List o indeksie Index oraz parametrze Elem.
%% Wynik tej funkcji jest podmieniany jako nowy element o tym indeksie.
-spec mapIndex(Elem::term(), Index::integer(), List::[term()], F::fun()) -> [term()].
mapIndex(Elem,Index,List,F) ->
    mapIndex(Elem,Index,List,F,[]).

%% @doc Funkcja czyszczaca skrzynke.
-spec clearInbox() -> ok.
clearInbox() ->
    receive
        _ -> clearInbox()
    after 0 ->
            ok
    end.

-spec shortestZip(list(),list()) -> list().
shortestZip(L1,L2) ->
    shortestZip(L1,L2,[]).

%% @doc Funkcja wyznaczajaca indeks pod jakim znajduje sie dany element na podanej liscie.
-spec find(term(),[term()]) -> integer().
find(Elem,List) ->
    find(Elem,List,1).

%% @doc Funkcja okreslajaca najlepszy wynik na podstawie przeslanej listy agentow
-spec result([term()]) -> float() | islandEmpty.
result([]) ->
    islandEmpty;

result(Agents) ->
    lists:max([ Fitness || {_ ,Fitness, _} <- Agents]).

-spec seedRandom() -> {integer(),integer(),integer()}.
seedRandom() ->
    {_,B,C} = erlang:now(),
    Hash = erlang:phash2(node()),
    random:seed(Hash,B,C).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec onlineCount({float(),float(),float()},integer(),fun()) -> {float(),float()}.
onlineCount({Xn, Mean, M}, N, F) ->
    NewMean = F(Mean,(Xn - Mean) / N),
    {NewMean, F(M,(Xn - Mean) * (Xn - NewMean))}.

%% @doc Funkcja wyznaczajaca indeks pod jakim znajduje sie dany element na podanej liscie.
-spec find(term(),[term()],integer()) -> integer().
find(Elem,[Elem|_],Inc) ->
    Inc;
find(_,[],_) ->
    notFound;
find(Elem,[_|T],Inc) ->
    find(Elem,T,Inc+1).

%% @doc Funkcja wykonuje funkcje F na elemencie listy List o indeksie Index oraz parametrze Elem.
%% Wynik tej funkcji jest podmieniany jako nowy element o tym indeksie.
-spec mapIndex(Elem::term(), Index::integer(), List::[term()], F::fun(), Acc::[term()]) -> [term()].
mapIndex(_,_,[],_,_) ->
    erlang:error(wrongIndex);
mapIndex(Elem,1,[H|T],F,Acc) ->
    lists:reverse(Acc,[F(Elem,H)|T]);
mapIndex(Elem,Index,[H|T],F,Acc) ->
    mapIndex(Elem,Index - 1,T,F,[H|Acc]).


%% @doc Funkcja wyliczajaca odchylenie standardowe rozkladu zadanego przez liste liczb
-spec variance([float()]) -> float().
variance([_Sol]) ->
    0.0;
variance(L) ->
    {Sum, Len} = lists:foldl(fun (X, {S,N}) ->
                                     {S+X,N+1}
                             end, {0,0}, L),
    Mean = Sum/Len,
    {Sum2} = lists:foldl(fun (X, {K}) ->
                                 {K+(X-Mean)*(X-Mean)}
                         end, {0}, L),
    %%     Variance =
    Sum2/(Len-1).
%%     math:sqrt(Variance).
%%     Variance = logBadValues(Sum2/(Len-1),Sum2, Len, Mean),
%%     math:sqrt(SqSum/Len-Mean*Mean).

%% logBadValues(Variance, Sum, Len, Mean) when Variance < 0 ->
%%     io:format("VARIANCE: ~p sum: ~p  N: ~p Mean: ~p ~n",[Variance, Sum, Len, Mean]),
%%     0;
%%
%% logBadValues(Variance, _Sum, _Len, _Mean) ->
%%     Variance.

%% @doc Funkcja wykonujaca operacje zip na dowolnej liczbie list wejsciowych
%% http://erlang.org/pipermail/erlang-questions/2012-October/069856.html
-spec transpose([[float()]]) -> [[float()]].
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]] | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [];
transpose(Tuple) when is_tuple(Tuple) ->  % wrapper to emulate zip
    Xs = transpose(tuple_to_list(Tuple)),
    [list_to_tuple(X) || X <- Xs].

shortestZip([],_L2,Acc) ->
    Acc;

shortestZip(_L1,[],Acc) ->
    Acc;

shortestZip([H1|T1],[H2|T2],Acc) ->
    shortestZip(T1,T2,[{H1,H2}|Acc]).