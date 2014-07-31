%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module contains helpers for writing to files and printing to stdout
%% Functions are mostly use-case dependent.
%% This module is deprecated

-module(io_util).
-export([printSeq/1, printMoreStats/1, genPath/4, sumEnergy/1, printArenas/1]).

-include ("mas.hrl").

-type groups() :: [{agent_behaviour(),[agent()]}].

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Prints basic stats for the sequential model. 
-spec printSeq([island()]) -> ok.
printSeq([]) -> ok;
printSeq([Island|T]) ->
    io:format("Island ~p Fitness ~p Population ~p Energy ~p~n",[length(T),misc_util:result(Island),length(Island),sumEnergy(Island)]),
    printSeq(T).

%% @doc Computes energy sum for the population
-spec sumEnergy([agent()]) -> integer().
sumEnergy(Agents) ->
    lists:foldl(fun({_,_,E},Acc) -> Acc + E end,0,Agents).

%% @doc Prints advanced stats for the sequential model
-spec printMoreStats(groups()) -> any().
printMoreStats(Groups) ->
    D = lists:flatten([X || {death,X} <- Groups]),
    F = lists:flatten([X || {fight,X} <- Groups]),
    R = lists:flatten([X || {reproduction,X} <- Groups]),
    M = lists:flatten([X || {migration,X} <- Groups]),
    io:format("Dying: ~p    Fighting: ~p    Reproducing: ~p    Leaving: ~p~n",[length(D),length(F),length(R),length(M)]),
    io:format("Population: ~p, Energy: ~p~n",[length(D ++ M ++ F ++ R),sumEnergy(M ++ F ++ R)]).

-spec printArenas([{atom(),pid()}]) -> ok.
printArenas([]) ->
    io:format("#supervisor: ~p~n~n",[self()]);
printArenas([{Name,Pid}|Arenas]) ->
    io:format("#~p: ~p~n",[Name,Pid]),
    printArenas(Arenas).


-spec genPath(string(),pos_integer(),pos_integer(),pos_integer()) -> string().
genPath(AlgType,Problem,TTime,Islands) ->
    catch file:make_dir(AlgType),
    Time = TTime div 1000,
    Param = integer_to_list(Problem) ++ "_" ++ integer_to_list(Time) ++ "_" ++ integer_to_list(Islands),
    Path = filename:join([AlgType, Param]),
    catch file:make_dir(Path),
    {ok,Instances} = file:list_dir(Path),
    Path2 =  filename:join([Path,assignName(Instances,0)]),
    file:make_dir(Path2),
    Path2.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec assignName([string()],integer()) -> string().
assignName(Files,N) ->
    Name = "instance" ++ integer_to_list(N),
    case lists:member(Name,Files) of
        true -> assignName(Files,N+1);
        false -> Name
    end.