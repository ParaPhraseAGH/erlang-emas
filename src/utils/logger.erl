%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer logger odpowiada za pisanie statystyk do plikow
-module(logger).
-behaviour(gen_server).

%% API
-export([start_link/2, logLocalStats/3, logGlobalStats/2, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link({atom(), [pid()] | integer()}, string()) -> {ok, pid()}.
start_link(Model, Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Model, Path], []).

%% @doc Loguje statystyki specyficzne dla danej wyspy (np. fitness, population).
%% Pierwsza zmienna informuje o trybie zapisu: parallel dla modelu concurrent i hybrid, a sequential dla sekwencyjnych.
-spec logLocalStats(sequential | parallel, atom(), term()) -> ok.
logLocalStats(Mode, Stat, Value) ->
    gen_server:cast(whereis(?MODULE), {Mode, Stat, self(), Value}).

%% @doc Zapisuje globalne statystyki (deaths,fights etc.) do plikow.
-spec logGlobalStats(sequential | parallel, tuple()) -> ok.
logGlobalStats(sequential, Counter) ->
    gen_server:cast(whereis(?MODULE), {counter, Counter});
logGlobalStats(parallel, Counter) ->
    gen_server:cast(whereis(?MODULE), {agregate, self(), Counter}).

-spec close() -> ok.
close() ->
    gen_server:cast(whereis(?MODULE), close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {dict :: dict(),
                counters = {0, 0, 0, 0} :: {integer(), integer(), integer(), integer()},
                bestFitness = -999999 :: integer(),
                populations = [] :: list(),
                n = 0 :: non_neg_integer()}).
-type state() :: #state{}.

-spec init(term()) -> {ok,state()}.
init([Model, Path]) ->
    Dict = case Model of
               {sequential, IslandsNr} ->
                   prepareSeqDictionary(IslandsNr, dict:new(), Path);
               {parallel, Pids} when is_list(Pids) ->
                   prepareParDictionary(Pids, dict:new(), Path)
           end,
    {ok, #state{dict = Dict}}.

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
handle_cast({parallel, Stat, _Pid, Value}, State) ->
    %%     logLocal(State#state.dict, Pid, Stat, Value),
    case Stat of
        fitness ->
            if State#state.bestFitness < Value ->
                    {noreply,State#state{bestFitness = Value}};
               State#state.bestFitness >= Value ->
                    {noreply,State}
            end;

        population ->
            N = length(dict:to_list(State#state.dict)) - 4,
            if length(State#state.populations) + 1 == N ->
                    io:format("fitness: ~p~n",[State#state.bestFitness]),
                    io:format("population: ~p~n",[lists:sum([Value|State#state.populations])]),
                    {noreply,State#state{populations = []}};
               length(State#state.populations) + 1 =/= N ->
                    OldPopulations = State#state.populations,
                    {noreply,State#state{populations = [Value|OldPopulations]}}
            end;
        _ ->
            error("No such stat!")

    end;
handle_cast({sequential, Stat, _Pid, Values}, State) ->
    %%     logList(Stat, 1, Values, State#state.dict),
    io:format(atom_to_list(Stat) ++ ": ~p~n",[lists:max(Values)]),
    {noreply, State};
handle_cast({counter, {Deaths, Fights, Reproductions, Migrations}}, State) ->
    %%     Dict = State#state.dict,
    %%     logGlobal(Dict, death, Deaths),
    %%     logGlobal(Dict, fight, Fights),
    %%     logGlobal(Dict, reproduction, Reproductions),
    %%     logGlobal(Dict, migration, Migrations),
    io:format("fights: ~p~n",[Fights]),
    io:format("reproductions: ~p~n",[Reproductions]),
    io:format("deaths: ~p~n",[Deaths]),
    io:format("migrations: ~p~n~n",[Migrations]),
    {noreply, State};
handle_cast({agregate, _Pid, Counters}, State) ->
    N = length(dict:to_list(State#state.dict)) - 4,
    case State#state.n + 1 of
        N ->
            logGlobalStats(sequential, addCounters(Counters, State#state.counters)),
            {noreply, #state{dict = State#state.dict}};
        X ->
            OldCounters = State#state.counters,
            {noreply, State#state{n = X, counters = addCounters(Counters, OldCounters)}}
    end;
handle_cast(close, State) ->
    {stop, normal, State}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, State) ->
    closeFiles(State#state.dict).

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow niesekwencyjnych
-spec prepareParDictionary([pid()], dict(), string()) -> dict().
prepareParDictionary([], Dict, Path) ->
    createFDs(Path, Dict, [death, fight, reproduction, migration]);
prepareParDictionary([H|T], Dict, Path) ->
    IslandPath = filename:join([Path, "island" ++ integer_to_list(length(T) + 1)]),
    file:make_dir(IslandPath),
    NewDict = dict:store(H, createFDs(IslandPath, dict:new(), [fitness, population]), Dict), % Key = pid(), Value = dictionary of file descriptors
    prepareParDictionary(T, NewDict, Path).

%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow sekwencyjnych
-spec prepareSeqDictionary(non_neg_integer(), dict(), string()) -> dict().
prepareSeqDictionary(0, Dict, Path) ->
    createFDs(Path, Dict, [death, fight, reproduction, migration]);
prepareSeqDictionary(IslandNr, Dict, Path) ->
    IslandPath = filename:join([Path, "island" ++ integer_to_list(IslandNr)]),
    file:make_dir(IslandPath),
    NewDict = dict:store(IslandNr, createFDs(IslandPath, dict:new(), [fitness, population]), Dict), % Key = IslandNumber, Value = dictionary of file descriptors
    prepareSeqDictionary(IslandNr - 1, NewDict, Path).

%% @doc Tworzy pliki tekstowe do zapisu i zwraca dict() z deskryptorami.
-spec createFDs(string(), dict(), [atom()]) -> FDs :: dict().
createFDs(Path, InitDict, Files) ->
    lists:foldl(fun(Atom, Dict) ->
                        Filename = atom_to_list(Atom) ++ ".txt",
                        {ok, Descriptor} = file:open(filename:join([Path, Filename]), [append, delayed_write, raw]),
                        dict:store(Atom, Descriptor, Dict)
                end, InitDict,
                Files).

-spec logList(atom(), pos_integer(), [term()], dict()) -> ok.
logList(_, _, [], _) ->
    ok;
logList(Stat, Index, [H|T], Dict) ->
    logLocal(Dict, Index, Stat, H),
    logList(Stat, Index + 1, T, Dict).

%% @doc Dokonuje buforowanego zapisu do pliku lokalnej statystyki. W argumencie podany glowny slownik, klucz, nazwa statystyki i wartosc do wpisania.
-spec logLocal(dict(), term(), atom(), term()) -> ok.
logLocal(Dictionary, Key, Statistic, Value) ->
    FDs = dict:fetch(Key, Dictionary),
    FD = dict:fetch(Statistic, FDs),
    file:write(FD, io_lib:fwrite("~p\n", [Value])).

%% @doc Dokonuje buforowanego zapisu do pliku globalnej statystyki. W argumencie podany glowny slownik, nazwa statystyki i wartosc do wpisania.
-spec logGlobal(dict(), atom(), term()) -> ok.
logGlobal(Dictionary, Stat, Value) ->
    FD = dict:fetch(Stat, Dictionary),
    file:write(FD, io_lib:fwrite("~p\n", [Value])).

%% @doc Zamyka pliki podane w argumencie
-spec closeFiles(dict()) -> any().
closeFiles(Dict) ->
    [case X of
         {Id, FD} when is_atom(Id) -> file:close(FD);
         {_Id, D} -> [file:close(FD) || {_Stat, FD} <- dict:to_list(D)]
     end || X <- dict:to_list(Dict)].

-spec addCounters(tuple(), tuple()) -> tuple().
addCounters({D1, F1, R1, M1}, {D2, F2, R2, M2}) ->
    {D1 + D2, F1 + F2, R1 + R2, M1 + M2}.
