%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer logger odpowiada za pisanie statystyk do plikow
-module(logger).
-behaviour(gen_server).

%% API
-export([start_link/2, logStat/3, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATS, [fitness, population, stddevsum, stddevmin, stddevvar]).
%% -define(GLOBAL_STATS, [death, fight, reproduction, migration]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link({atom(), [pid()] | integer()}, string()) -> {ok, pid()}.
start_link(Keys, Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Keys, Path], []).

%% %% @doc Loguje statystyki specyficzne dla danej wyspy (np. fitness, population).
%% %% Pierwsza zmienna informuje o trybie zapisu: parallel dla modelu concurrent i hybrid, a sequential dla sekwencyjnych.
%% -spec logLocalStats(sequential | parallel, atom(), term()) -> ok.
%% logLocalStats(Mode, Stat, Value) ->
%%     gen_server:cast(whereis(?MODULE), {Mode, Stat, self(), Value}).
%%
%% %% @doc Zapisuje globalne statystyki (deaths,fights etc.) do plikow.
%% -spec logGlobalStats(sequential | parallel, dict()) -> ok.
%% logGlobalStats(sequential, Counter) ->
%%     gen_server:cast(whereis(?MODULE), {counter, Counter});
%%
%% logGlobalStats(parallel, Counter) ->
%%     gen_server:cast(whereis(?MODULE), {agregate, self(), Counter}).

-spec logStat(term(),atom(),term()) -> ok.
logStat(Key,Stat,Value) ->
    gen_server:cast(whereis(?MODULE), {stat, Key, Stat, Value}).

-spec close() -> ok.
close() ->
    gen_server:cast(whereis(?MODULE), close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {fds :: dict:dict(),
                counters = dict:new() :: dict:dict(),
                stats = [] :: [atom()]}).
-type state() :: #state{}.

-spec init(term()) -> {ok,state()}.
init([Keys, Path]) ->
    Dict = prepareDictionary(Keys, dict:new(), Path),
    Stats = misc_util:determineStats(),
    timer:send_interval(config:writeInterval(),timer),
    {ok, #state{fds = Dict, stats = Stats, counters = createCounter(Keys)}}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({stat, Key, Stat, Value}, State) ->
    IslandDict = dict:fetch(Key,State#state.counters),
    NewCounter = dict:store(Stat,Value,IslandDict),
    {noreply,State#state{counters = dict:store(Key,NewCounter,State#state.counters)}};

%% handle_cast({parallel, Stat, Pid, Value}, State) ->
%%     logLocal(State#state.fds, Pid, Stat, Value),
%%     {noreply,State};
%%
%% handle_cast({sequential, Stat, _Pid, Values}, State) ->
%%     logList(Stat, 1, Values, State#state.fds),
%%     {noreply,State};
%%
%% handle_cast({counter, GlobalStats}, State) ->
%%     [logGlobal(State#state.fds,StatName,StatVal) || {StatName,StatVal} <- dict:to_list(GlobalStats)],
%%     {noreply, State};
%%
%% handle_cast({agregate, _Pid, Counters}, State) ->  % todo przepisac, zeby odroznial wyspy
%%     N = dict:size(State#state.fds) - 4,
%%     case State#state.n + 1 of
%%         N ->
%%             logGlobalStats(sequential, addCounters(Counters, State#state.counters)),
%%             {noreply, State#state{n = 0, counters = misc_util:createNewCounter()}};
%%         X ->
%%             OldCounters = State#state.counters,
%%             {noreply, State#state{n = X, counters = addCounters(Counters, OldCounters)}}
%%     end;

handle_cast(close, State) ->
    {stop, normal, State}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timer, State = #state{fds = FDs, counters = Counters, stats = Stats}) ->
    % TODO
    %%     io:format("Tick!~n"),
    Acc = gatherStats(BigDict,Stats),
    [logGlobal(FDs,X,dict:fetch(X,Acc)) || X <- Stats],
%%     NewBigDict = dict:fold(fun(Pid,LocalDict,NewDict) ->
%%                                    PopulationChange = dict:fetch(reproduction,LocalDict) + dict:fetch(immigration,LocalDict)
%%                                        - dict:fetch(death,LocalDict) - dict:fetch(emigration,LocalDict),
%%                                    UpdatePopulation = dict:update_counter(population,PopulationChange,LocalDict),
%%                                    WithZeros = lists:foldl(fun(Stat,TMPDict) ->
%%                                                                    dict:store(Stat,0,TMPDict)
%%                                                            end,UpdatePopulation,[reproduction,death,fight,emigration,immigration]),
%%                                    dict:store(Pid,WithZeros,NewDict)
%%                            end,dict:new(),BigDict),
%%     dict:merge(fun(Pid,LocalDict,FDDict) ->
%%                        [logLocal(FDDict,Pid,X,dict:fetch(X,LocalDict)) || X <- ?LOCAL_STATS]
%%                end,NewBigDict,FDs),
    NewBigDict = createCounter(dict:fetch_keys(BigDict),Stats),
    {noreply, State#state{counters = NewBigDict},State#state.timeout};

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, State) ->
    closeFiles(State#state.fds).

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec prepareDictionary([term()],dict:dict(),string()) -> dict().
prepareDictionary([], Dict, _Path) ->
    Dict;

prepareDictionary([Key|Rest],Dict,Path) ->
    IslandPath = createDir(Path,length(Rest) + 1),
    NewDict = dict:store(Key, createFDs(IslandPath, dict:new(), ?STATS), Dict),
    prepareDictionary(Rest, NewDict, Path).

-spec createDir(string(), pos_integer()) -> standard_io | string().
createDir("standard_io",_IslandsNr) ->
    standard_io;

createDir(Path, IslandsNr) ->
    NewPath = filename:join([Path, "island" ++ integer_to_list(IslandsNr)]),
    file:make_dir(NewPath),
    NewPath.

%% @doc Tworzy pliki tekstowe do zapisu i zwraca dict:dict() z deskryptorami.
-spec createFDs(string(), dict:dict(), [atom()]) -> FDs :: dict:dict().
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

-spec createCounter(list()) -> dict:dict().
createCounter(Keys) ->
    Environment = config:agent_env(),
    Interactions = [{Interaction,0} || Interaction <- Environment:behaviours()],
    Stats = [{Stat,InitValue} || {Stat,_Fun,InitValue} <- Environment:stats()],
    IslandDict = dict:from_list(Interactions ++ Stats),
    lists:foldl(fun(Key,Dict) ->
                        dict:store(Key,IslandDict,Dict)
                end, dict:new(), Keys).


-spec logList(atom(), pos_integer(), [term()], dict:dict()) -> ok.
logList(_, _, [], _) ->
    ok;
logList(Stat, Index, [H|T], Dict) ->
    logLocal(Dict, Index, Stat, H),
    logList(Stat, Index + 1, T, Dict).

%% @doc Dokonuje buforowanego zapisu do pliku lokalnej statystyki. W argumencie podany glowny slownik, klucz, nazwa statystyki i wartosc do wpisania.
-spec logLocal(dict:dict(), term(), atom(), term()) -> ok.
logLocal(Dictionary, Key, Statistic, Value) ->
    FDs = dict:fetch(Key, Dictionary),
    FD = dict:fetch(Statistic, FDs),
    file:write(FD, io_lib:fwrite("~p ~p ~p\n", [Statistic, Key, Value])).

%% @doc Dokonuje buforowanego zapisu do pliku globalnej statystyki. W argumencie podany glowny slownik, nazwa statystyki i wartosc do wpisania.
-spec logGlobal(dict:dict(), atom(), term()) -> ok.
logGlobal(Dictionary, Stat, Value) ->
    FD = dict:fetch(Stat, Dictionary),
    file:write(FD, io_lib:fwrite("~p ~p\n", [Stat,Value])).

%% @doc Zamyka pliki podane w argumencie
-spec closeFiles(dict:dict()) -> any().
closeFiles(Dict) ->
    [case X of
         {Id, FD} when is_atom(Id) -> file:close(FD);
         {_Id, D} -> [file:close(FD) || {_Stat, FD} <- dict:to_list(D)]
     end || X <- dict:to_list(Dict)].

-spec addCounters(dict:dict(), dict:dict()) -> dict:dict().
addCounters(C1,C2) ->
    dict:fold(fun(Key, Value, TmpDict) ->
                      dict:update_counter(Key,Value,TmpDict)
              end, C1, C2).
