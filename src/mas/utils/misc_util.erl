%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module contains common helpers for different models of computation

-module(misc_util).
-export([groupBy/1, shuffle/1, clearInbox/0, result/1, find/2, averageNumber/2, mapIndex/4, shortestZip/2, count_funstats/2,
         seedRandom/0, logNow/1, meeting_proxy/2, create_new_counter/0, add_interactions_to_counter/2, determineStats/0, add_miliseconds/2]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Groups tuples as following:
%% [{K1,V1},{K2,V2},...] -> [{K1,[V1,V3]},{K2,[V2,V4,V5]},...]
-spec groupBy([{term(),term()}]) -> [{term(),[term()]}].
groupBy(List) ->
    dict:to_list(
      lists:foldl(fun({K,V}, D) ->
                          dict:append(K, V, D)
                  end , dict:new(), List)).


-spec shuffle(list()) -> list().
shuffle(List) ->
    Rand = [{random:uniform(), N} || N <- List],
    [X || {_, X} <- lists:sort(Rand)].


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


%% @doc Computes an average number of elements that are chosen with given probability
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
    IntervalInMicros = config:writeInterval() * 1000,
    if
        Diff >= IntervalInMicros ->
            {Mega,Sec,Micro} = LastLog,
            {yes,{Mega,Sec+1,Micro}};
        true ->
            notyet
    end.


-spec create_new_counter() -> counter().
create_new_counter() ->
    Environment = config:agent_env(),
    BehaviourList = [{Behaviour,0} || Behaviour <- Environment:behaviours()],
    dict:from_list(BehaviourList).


-spec add_interactions_to_counter([tuple()], counter()) -> counter().
add_interactions_to_counter(Groups, Counter) ->
    lists:foldl(fun({Activity, Value}, TmpCounter) ->
                        dict:update_counter(Activity, length(Value), TmpCounter)
                end,Counter, Groups).

-spec count_funstats([agent()], [funstat()]) -> [funstat()].
count_funstats(_,[]) ->
    [];

count_funstats(Agents, [{Stat, MapFun, ReduceFun, OldAcc}|T]) ->
    NewAcc = lists:foldl(ReduceFun,
        OldAcc,
        [MapFun(Agent) || Agent <- Agents]),
    [{Stat, MapFun, ReduceFun, NewAcc} | count_funstats(Agents,T)].

-spec add_miliseconds({integer(),integer(),integer()},integer()) -> {integer(),integer(),integer()}.
add_miliseconds({MegaSec, Sec, Milisec}, Time) ->
    {MegaSec,
        Sec + (Time div 1000),
        Milisec + (Time rem 1000)}.

%% @doc Maps function F(Elem, A) -> A' to an element A of List with Index. Returns updated list
-spec mapIndex(Elem::term(), Index::integer(), List::[term()], F::fun()) -> [term()].
mapIndex(Elem,Index,List,F) ->
    mapIndex(Elem,Index,List,F,[]).


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

%% @doc Returns the index of a given element in a given list
-spec find(term(),[term()]) -> integer() | 'notFound'.
find(Elem,List) ->
    find(Elem,List,1).


%% @doc This function is use case dependent and deprecated
-spec result([term()]) -> float() | 'islandEmpty'.
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

-spec find(term(),[term()],integer()) -> integer() | 'notFound'.
find(Elem,[Elem|_],Inc) ->
    Inc;
find(_,[],_) ->
    notFound;
find(Elem,[_|T],Inc) ->
    find(Elem,T,Inc+1).


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