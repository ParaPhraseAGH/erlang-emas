%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami pomocniczymi dla roznych wersji algorytmu.

-module(misc_util).
-export([groupBy/1, shuffle/1, clearInbox/0, result/1, find/2, averageNumber/2, mapIndex/4, shortestZip/2,
         seedRandom/0, logNow/1, meeting_proxy/2, createNewCounter/0, countInteractions/2, determineStats/0]).

-include ("mas.hrl").

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

-spec determineStats() -> [atom()].
determineStats() ->
    Environment = config:agent_env(),
    Environment:behaviours().

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

-spec createNewCounter() -> dict:dict().
createNewCounter() ->
    Environment = config:agent_env(),
    BehaviourList = [{Behaviour,0} || Behaviour <- Environment:behaviours()],
    dict:from_list(BehaviourList).

-spec countInteractions([tuple()],dict:dict()) -> dict:dict().
countInteractions([],Counter) ->
    Counter;

countInteractions([Groups|T],Counter) ->
    UpdatedCounter = lists:foldl(fun({Activity,Value},TmpCounter) ->
        dict:update_counter(Activity,length(Value),TmpCounter)
    end,Counter,Groups),
    countInteractions(T,UpdatedCounter).

%% %% @doc Zwraca liczby agentow nalezacych do poszczegolnych kategorii w formie rekordu
%% -spec countGroups([tuple()],counter()) -> counter().
%% countGroups([],Counter) ->
%%     Counter;
%% countGroups([{death,AgentList}|Groups],Counter) ->
%%     countGroups(Groups,Counter#counter{death = length(AgentList)});
%% countGroups([{migration,AgentList}|Groups],Counter) ->
%%     countGroups(Groups,Counter#counter{migration = length(AgentList)});
%% countGroups([{fight,AgentList}|Groups],Counter) ->
%%     countGroups(Groups,Counter#counter{fight = length(AgentList)});
%% countGroups([{reproduction,AgentList}|Groups],Counter) ->
%%     countGroups(Groups,Counter#counter{reproduction = length(AgentList)}).
%%
%% %% @doc Funkcja dodaje dwa liczniki
%% -spec addCounters(counter(),counter()) -> counter().
%% addCounters(C1,C2) ->
%%     #counter{fight = C1#counter.fight + C2#counter.fight,
%%              reproduction = C1#counter.reproduction + C2#counter.reproduction,
%%              death = C1#counter.death + C2#counter.death,
%%              migration = C1#counter.migration + C2#counter.migration}.

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

shortestZip([],_L2,Acc) ->
    Acc;

shortestZip(_L1,[],Acc) ->
    Acc;

shortestZip([H1|T1],[H2|T2],Acc) ->
    shortestZip(T1,T2,[{H1,H2}|Acc]).