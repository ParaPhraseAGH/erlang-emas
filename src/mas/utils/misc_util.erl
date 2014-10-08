%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module contains common helpers for different models of computation

-module(misc_util).
-export([group_by/1, shuffle/1, clear_inbox/0, result/1, find/2, average_number/2, map_index/4, shortest_zip/2,
         count_funstats/2, seed_random/0, log_now/2, meeting_proxy/4, create_new_counter/1, add_interactions_to_counter/2, get_config_dir/0,
         add_miliseconds/2, generate_population/2, overwrite_options/2, determine_behaviours/1]).

-include ("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Groups tuples as following:
%% [{K1,V1},{K2,V2},...] -> [{K1,[V1,V3]},{K2,[V2,V4,V5]},...]
-spec group_by([{term(),term()}]) -> [{term(),[term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K,V}, D) ->
                          dict:append(K, V, D)
                  end , dict:new(), List)).


-spec shuffle(list()) -> list().
shuffle(L) ->
    Rand = [{random:uniform(), N} || N <- L],
    [X||{_,X} <- lists:sort(Rand)].


%% @doc Generates a population of agents with random solutions.
-spec generate_population(sim_params(), config()) -> [agent()].
generate_population(SP, Cf) ->
    Env = Cf#config.agent_env,
    [Env:initial_agent(SP) || _ <- lists:seq(1, Cf#config.population_size)].


-spec meeting_proxy({atom(), list()}, model(), sim_params(), config()) -> list().
meeting_proxy({migration, _Agents}, sequential, _SimParams, _Config) ->
    [];

meeting_proxy({migration, Agents}, hybrid, _SimParams, _Config) ->
    [hybrid:sendAgent(Agent) || Agent <- Agents],
    [];

meeting_proxy({migration, _Agents}, concurrent, _SimParams, _Config) ->
    [];

meeting_proxy({migration, Agents}, skel, _SimParams, _Config) ->
    Agents;

meeting_proxy(Group, _, SP, #config{agent_env = Env}) ->
    Env:meeting_function(Group, SP).


-spec determine_behaviours(config()) -> [agent_behaviour() | migration].
determine_behaviours(#config{agent_env = Env}) ->
    [migration | Env:behaviours()].


%% @doc Computes an average number of elements that are chosen with given probability
-spec average_number(float(),[term()]) -> integer().
average_number(Probability, List) ->
    N = Probability * length(List),
    if N == 0 -> 0;
       N < 1 ->
            case random:uniform() < N of
                true -> 1;
                false -> 0
            end;
       N >=1 -> trunc(N)
    end.


-spec log_now(erlang:timestamp(), config()) -> {yes,erlang:timestamp()} | notyet.
log_now(LastLog, #config{write_interval = WriteInterval}) ->
    Now = os:timestamp(),
    Diff = timer:now_diff(Now, LastLog),
    IntervalInMicros = WriteInterval * 1000,
    if
        Diff >= IntervalInMicros ->
            {Mega, Sec, Micro} = LastLog,
            {yes,{Mega, Sec + 1, Micro}};
        true ->
            notyet
    end.


-spec create_new_counter(config()) -> counter().
create_new_counter(Config) ->
    BehaviourList = [{Behaviour,0} || Behaviour <- determine_behaviours(Config)],
    dict:from_list(BehaviourList).


-spec add_interactions_to_counter([tuple()], counter()) -> counter().
add_interactions_to_counter(Groups, Counter) ->
    lists:foldl(fun({Activity, Value}, TmpCounter) ->
                        dict:update_counter(Activity, length(Value), TmpCounter)
                end,Counter, Groups).


%% @doc Overwrites the parameters in a proplist
%%      with the keyvals from another proplist
-spec overwrite_options([tuple()],[tuple()]) -> [tuple()].
overwrite_options([], Overwritten) ->
    Overwritten;

overwrite_options([{Key, Val}|OtherOptions], ToBeOverwritten) ->
    overwrite_options(OtherOptions, lists:keyreplace(Key, 1, ToBeOverwritten, {Key, Val})).


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
-spec map_index(Elem::term(), Index::integer(), List::[term()], F::fun()) -> [term()].
map_index(Elem,Index,List,F) ->
    map_index(Elem,Index,List,F,[]).


-spec clear_inbox() -> ok.
clear_inbox() ->
    receive
        _ -> clear_inbox()
    after 0 ->
            ok
    end.


-spec shortest_zip(list(),list()) -> list().
shortest_zip(L1,L2) ->
    shortest_zip(L1,L2,[]).


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


-spec seed_random() -> {integer(),integer(),integer()}.
seed_random() ->
    {_,B,C} = erlang:now(),
    Hash = erlang:phash2(node()),
    random:seed(Hash,B,C).

-spec get_config_dir() -> file:filename_all().
get_config_dir() ->
    AppDir = filename:dirname(filename:dirname(code:which(?MODULE))),
    filename:join(AppDir, "etc").

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


-spec map_index(Elem::term(), Index::integer(), List::[term()], F::fun(), Acc::[term()]) -> [term()].
map_index(_,_,[],_,_) ->
    erlang:error(wrongIndex);

map_index(Elem,1,[H|T],F,Acc) ->
    lists:reverse(Acc,[F(Elem,H)|T]);

map_index(Elem,Index,[H|T],F,Acc) ->
    map_index(Elem,Index - 1,T,F,[H|Acc]).


-spec shortest_zip(list(), list(), list()) -> list().
shortest_zip([],_L2,Acc) ->
    Acc;

shortest_zip(_L1,[],Acc) ->
    Acc;

shortest_zip([H1|T1],[H2|T2],Acc) ->
    shortest_zip(T1,T2,[{H1,H2}|Acc]).