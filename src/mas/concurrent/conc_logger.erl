%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer logger odpowiada za pisanie statystyk do plikow
-module(conc_logger).
-behaviour(gen_server).

%% API
-export([start_link/2, log/3, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOCAL_STATS, [fitness, population, stddevsum, stddevmin, stddevvar]).
%% -define(GLOBAL_STATS, [death, fight, reproduction, migration]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link([pid()],string()) -> {ok, pid()}.
start_link(Supervisors,Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Supervisors,Path], []).

-spec log(pid(),atom(),term()) -> ok.
log(Supervisor,Arena,Value) ->
    gen_server:cast(whereis(?MODULE),{report,Arena,Supervisor,Value}).

-spec close() -> ok.
close() ->
    gen_server:cast(whereis(?MODULE), close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {fds = undefined :: dict:dict(),
                counters = undefined :: dict:dict(),
                stats = [] :: [atom()],
                timeout = infinity :: timeout()}).
-type state() :: #state{}.

-spec init(term()) -> {ok,state()}.
init([Supervisors,Path]) ->
    self() ! delayTimerStart,
    NewPath = case Path of
                  "standard_io" -> standard_io;
                  X -> X
              end,
    Stats = misc_util:determineStats(),
    FDs = prepareParDictionary(Supervisors, dict:new(), NewPath,Stats),
    Counters = createCounter(Supervisors,Stats),
    {ok,#state{counters = Counters, fds = FDs, stats = Stats}}.


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

handle_cast({report,migration,Supervisor,{Emigrants,Immigrants}},State) ->
%%         io:format("migration~n"),
    LocalDict = dict:fetch(Supervisor,State#state.counters),
    AddImmigrants = dict:update_counter(immigration,Immigrants,LocalDict),
    AddEmigrants = dict:update_counter(emigration,Emigrants,AddImmigrants),
    {noreply,State#state{counters = dict:store(Supervisor,AddEmigrants,State#state.counters)},State#state.timeout};

%% handle_cast({report,reproduction,Supervisor,{BestFitness,Reproductions}},State) ->
%%         io:format("reproduction~n"),
%%     LocalDict = dict:fetch(Supervisor,State#state.counters),
%%     AddReproductions = dict:update_counter(reproduction,Reproductions,LocalDict),
%%     UpdateFitness = dict:update(fitness,
%%                                 fun(OldFit) -> max(OldFit,BestFitness) end,
%%                                 AddReproductions),
%%     {noreply,State#state{counters = dict:store(Supervisor,UpdateFitness,State#state.counters)},State#state.timeout};

%% handle_cast({report,diversity,Supervisor,{VarSum,VarMin,VarVar}},State) ->
%%         io:format("diversity ~n"),
%%     LocalDict = dict:fetch(Supervisor,State#state.counters),
%%     AddSum = dict:store(stddevsum,VarSum,LocalDict),
%%     AddMin = dict:store(stddevmin,VarMin,AddSum),
%%     AddVar = dict:store(stddevvar,VarVar,AddMin),
%%     {noreply,State#state{counters = dict:store(Supervisor,AddVar,State#state.counters)},State#state.timeout};

handle_cast({report,Arena,Supervisor,Value},State) ->
%%         io:format("~p ~p~n",[Arena,Value]),
    LocalDict = dict:fetch(Supervisor,State#state.counters),
    AddValue = dict:update_counter(Arena,Value,LocalDict),
    {noreply,State#state{counters = dict:store(Supervisor,AddValue,State#state.counters)},State#state.timeout};

handle_cast(close, State) ->
    NewTimeout = trunc(config:writeInterval() * 0.9),
    {noreply, State#state{timeout = NewTimeout},NewTimeout}.
%%     {stop,normal,State}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
%% handle_info(tick, State) ->
%%     io:format(".~n"),
%%     {noreply,State,State#state.timeout};

handle_info(timer, State = #state{fds = FDs, counters = BigDict, stats = Stats}) ->
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


handle_info(delayTimerStart, State) ->
    timer:sleep(700),
    %%     timer:send_interval(50,tick),
    timer:send_interval(config:writeInterval(),timer),
    {noreply, State,State#state.timeout};

handle_info(timeout, State) ->
    {stop,normal,State}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, State) ->
    closeFiles(State#state.fds).

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec gatherStats(dict:dict(),[atom()]) -> dict:dict().
gatherStats(BigDict,Stats) ->
    Acc0 = dict:from_list([{X,0} || X <- Stats]),
    dict:fold(fun(_Pid,LocalDict,Acc) ->
                      lists:foldl(fun(Stat,InnerAcc) ->
                                          case Stat of
                                              migration ->
                                                  dict:update_counter(migration,dict:fetch(emigration,LocalDict),InnerAcc);
                                              _ ->
                                                  dict:update_counter(Stat,dict:fetch(Stat,LocalDict),InnerAcc)
                                          end
                                  end,Acc,Stats)
              end,Acc0,BigDict).

-spec createCounter([pid()],[atom()]) -> dict:dict().
createCounter(Pids,Stats) ->
    BasicStat = [{X,0} || X <- lists:delete(migration,Stats) ++ [emigration,immigration]],
    %%     Variance = [{X,-1} || X <-[stddevmin,stddevsum,stddevvar]],
    %%     FitnessPopulation = [{fitness,-999999.9},{population,config:populationSize()}],
    IslandDict = dict:from_list(BasicStat), % ++ Variance ++ FitnessPopulation),
    dict:from_list([{Pid,IslandDict} || Pid <- Pids]).

%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow niesekwencyjnych
-spec prepareParDictionary([pid()], dict:dict(), string(), [atom()]) -> dict:dict().
prepareParDictionary([], Dict, Path, Stats) ->
    createFDs(Path, Dict, Stats);

prepareParDictionary([H|T], Dict, Path, Stats) ->
    IslandPath = case Path of
                     standard_io ->
                         standard_io;
                     _ ->
                         NewPath = filename:join([Path, "island" ++ integer_to_list(length(T) + 1)]),
                         file:make_dir(NewPath),
                         NewPath
                 end,
    NewDict = dict:store(H, createFDs(IslandPath, dict:new(), ?LOCAL_STATS), Dict), % Key = pid(), Value = dictionary of file descriptors
    prepareParDictionary(T, NewDict, Path,Stats).

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

logGlobal(FDDict,Stat,Value) ->
    FD = dict:fetch(Stat, FDDict),
    file:write(FD, io_lib:fwrite("~p ~p\n", [Stat,Value])).

%% logLocal(FDDict,Supervisor,Stat,Value) ->
%%     FD = dict:fetch(Stat, FDDict),
%%     file:write(FD, io_lib:fwrite("~p ~p ~p\n", [Stat,Supervisor,Value])).

%% @doc Zamyka pliki podane w argumencie
-spec closeFiles(dict:dict()) -> any().
closeFiles(Dict) ->
    [case X of
         {Id, FD} when is_atom(Id) -> file:close(FD);
         {_Id, D} -> [file:close(FD) || {_Stat, FD} <- dict:to_list(D)]
     end || X <- dict:to_list(Dict)].
