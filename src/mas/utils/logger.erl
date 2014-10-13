%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles logging statistics of non-concurrent models

-module(logger).
-behaviour(gen_server).

-include ("mas.hrl").

%% API
-export([start_link/2, log_funstat/3, log_countstat/3, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%-define(STATS, [fitness, population, death, fight, reproduction, migration]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(list(), config()) -> {ok, pid()}.
start_link(Keys, Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Keys, Config], []).

-spec log_funstat(term(),atom(),term()) -> ok.
log_funstat(Key,Stat,Value) ->
    gen_server:cast(whereis(?MODULE), {funstat, Key, Stat, Value}).

-spec log_countstat(term(),atom(),term()) -> ok.
log_countstat(Key,Stat,Value) ->
    gen_server:cast(whereis(?MODULE), {countstat, Key, Stat, Value}).

-spec close() -> ok.
close() ->
    gen_server:call(whereis(?MODULE), close, infinity).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {fds :: dict:dict(),
                allstats = [] :: [atom()],
                funstats = [] :: [tuple()],
                counters = dict:new() :: dict:dict(),
                supervisor_from :: {pid(), term()},
                config :: config(),
                timeout = infinity :: infinity | non_neg_integer()}).
-type state() :: #state{}.

-spec init(term()) -> {ok,state()}.
init([Keys, Cf]) ->
    self() ! delayTimerStart,
    Env = Cf#config.agent_env,
    Funstats = Env:stats(),
    Stats = misc_util:determine_behaviours(Cf) ++ [Name || {Name, _MapFun, _ReduceFun, _InitVal} <- Funstats],
    Dict = prepareDictionary(lists:reverse(Keys), dict:new(), Cf#config.log_dir, Stats),
    {ok, #state{fds = Dict,
                funstats = Funstats,
                counters = create_counter(Keys, Cf),
                config = Cf,
                allstats = Stats}, infinity}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(close, From, St) ->
    Timeout = trunc(St#state.config#config.write_interval * 0.8),
    {noreply, St#state{timeout = Timeout, supervisor_from = From}, Timeout}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({funstat, Key, Stat, Value}, St) ->
    IslandDict = dict:fetch(Key, St#state.counters),
    OldVal = dict:fetch(Stat, IslandDict),
    {Stat, _Map, Reduce, _InitVal} = lists:keyfind(Stat, 1, St#state.funstats),
    NewVal = Reduce(OldVal, Value),
    NewIslandDict = dict:store(Stat, NewVal, IslandDict),
    {noreply, St#state{counters = dict:store(Key,NewIslandDict,St#state.counters)}, St#state.timeout};

handle_cast({countstat, Key, Stat, Value}, St) ->
    IslandDict = dict:fetch(Key, St#state.counters),
    DictUpdated = dict:update_counter(Stat, Value, IslandDict),
    {noreply,St#state{counters = dict:store(Key, DictUpdated, St#state.counters)}, St#state.timeout}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timer, St = #state{fds = FDs, counters = Counters, funstats = Funstats}) ->
    NewCounters = dict:map(fun(Key, CounterDict) ->
                                   FDDict = dict:fetch(Key, FDs),
                                   logIsland(Key, dict:to_list(FDDict), CounterDict, Funstats)
                           end, Counters),
    {noreply, St#state{counters = NewCounters}, St#state.timeout};

handle_info(delayTimerStart, St) ->
    timer:sleep(700),
    timer:send_interval(St#state.config#config.write_interval, timer),
    {noreply, St, St#state.timeout};

handle_info(timeout, St) ->
    gen_server:reply(St#state.supervisor_from,ok),
    {stop, normal, St}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, St) ->
    closeFiles(St#state.fds).

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec prepareDictionary([term()], dict:dict(), string(), [atom()]) -> dict:dict().
prepareDictionary([], Dict, _Path, _Stats) ->
    Dict;

prepareDictionary([Key|Rest], Dict, Path, Stats) ->
    IslandPath = createDir(Path,length(Rest) + 1),
    NewDict = dict:store(Key, createFDs(IslandPath, dict:new(), Stats), Dict),
    prepareDictionary(Rest, NewDict, Path, Stats).


-spec createDir(standard_io | string(), pos_integer()) -> standard_io | string().
createDir(standard_io, _IslandsNr) ->
    standard_io;

createDir("standard_io", _IslandsNr) ->
    standard_io;

createDir(Path, IslandsNr) ->
    NewPath = filename:join([Path, "island" ++ integer_to_list(IslandsNr)]),
    file:make_dir(NewPath),
    NewPath.


%% @doc Creates a dictionary with stat names as keys and corresponding file descriptors
-spec createFDs(standard_io | string(), dict:dict(), [atom()]) -> FDs :: dict:dict().
createFDs(standard_io, InitDict, Files) ->
    lists:foldl(fun(Atom, Dict) ->
                        dict:store(Atom, standard_io, Dict)
                end, InitDict, Files);

createFDs(Path, InitDict, Files) ->
    lists:foldl(fun(Atom, Dict) ->
                        Filename = atom_to_list(Atom) ++ ".txt",
                        {ok, Descriptor} = file:open(filename:join([Path, Filename]), [append, delayed_write, raw]),
                        dict:store(Atom, Descriptor, Dict)
                end, InitDict, Files).


-spec create_counter(list(), config()) -> dict:dict().
create_counter(Keys, Cf) ->
    Environment = Cf#config.agent_env,
    Interactions = [{Interaction, 0} || Interaction <- misc_util:determine_behaviours(Cf)],
    Stats = [{Stat, InitValue} || {Stat, _MapFun, _ReduceFun, InitValue} <- Environment:stats()],
    IslandDict = dict:from_list(Interactions ++ Stats),
    lists:foldl(fun(Key, Dict) ->
                        dict:store(Key, IslandDict, Dict)
                end, dict:new(), Keys).


-spec logIsland(pid() | pos_integer(), [tuple()], counter(), [funstat()]) -> counter().
logIsland(_Key, [], Counter, _Funstats) ->
    Counter;

logIsland(Key, [{Stat, FD}|FDs], Counter, Funstats) ->
    Value = dict:fetch(Stat, Counter),
    file:write(FD, io_lib:fwrite("~p ~p ~p\n", [Stat, Key, Value])),
    NewCounter = case lists:keyfind(Stat, 1, Funstats) of
                     false ->
                         dict:store(Stat, 0, Counter);
                     _Tuple ->
                         Counter
                 end,
    logIsland(Key, FDs, NewCounter, Funstats).


-spec closeFiles(dict:dict()) -> any().
closeFiles(Dict) ->
    [case X of
         {Id, FD} when is_atom(Id) -> file:close(FD);
         {_Id, D} -> [file:close(FD) || {_Stat, FD} <- dict:to_list(D)]
     end || X <- dict:to_list(Dict)].

