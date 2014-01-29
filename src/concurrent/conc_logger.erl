%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer logger odpowiada za pisanie statystyk do plikow
-module(conc_logger).
-behaviour(gen_server).

%% API
-export([start_link/2, log/3, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOCAL_STATS, []).
-define(GLOBAL_STATS, [death, fight, reproduction, migration, fitness, population]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link([pid()], string()) -> {ok, pid()}.
start_link(Pids, Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Pids, Path], []).

-spec log(atom(),pid(),term()) -> ok.
log(Arena,Supervisor,Value) ->
    gen_server:cast(whereis(?MODULE),{report,Arena,Supervisor,Value}).

-spec close() -> ok.
close() ->
    gen_server:cast(whereis(?MODULE), close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {fds :: dict(),
                counters = dict:new() :: dict(),
                n = 0 :: non_neg_integer()}).
-type state() :: #state{}.

-spec init(term()) -> {ok,state()}.
init([Pids, Path]) ->
    NewPath = case Path of
                  "standard_io" -> standard_io;
                  X -> X
              end,
    timer:send_interval(config:writeInterval(),timer),
    FDs = prepareParDictionary(Pids, dict:new(), NewPath),
    Counters = createCounter(-999999.9,config:populationSize()*length(Pids)),
    {ok, #state{fds = FDs, counters = Counters}}.


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

handle_cast({report,migration,_Supervisor,{Emigrants,Immigrants}},State) ->
    AddImmigrants = dict:update_counter(immigration,Immigrants,State#state.counters),
    AddEmigrants = dict:update_counter(emigration,Emigrants,AddImmigrants),
    {noreply,State#state{counters = AddEmigrants}};

handle_cast({report,reproduction,_Supervisor,{BestFitness,Reproductions}},State) ->
    AddReproductions = dict:update_counter(reproduction,Reproductions,State#state.counters),
    OldFitness = dict:fetch(fitness,State#state.counters),
    UpdateFitness = case BestFitness > OldFitness of
                        true ->
                            dict:store(fitness,BestFitness,AddReproductions);
                        false ->
                            AddReproductions
                    end,
    {noreply,State#state{counters = UpdateFitness}};

handle_cast({report,Arena,_Supervisor,Value},State) ->
    AddValue = dict:update_counter(Arena,Value,State#state.counters),
    {noreply,State#state{counters = AddValue}};

handle_cast(close, State) ->
    {stop, normal, State}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timer, State) ->
    Dict = State#state.counters,
    FDs = State#state.fds,
    [logGlobal(FDs,X,dict:fetch(X,Dict)) || X <- [reproduction,fight,death,fitness]],
    Migration = (dict:fetch(emigration,Dict) + dict:fetch(immigration,Dict)) div 2,
    logGlobal(FDs,migration,Migration),
    NewPopulation = dict:fetch(reproduction,Dict) - dict:fetch(death,Dict) + dict:fetch(population,Dict),
    logGlobal(FDs,population,NewPopulation),
    Best = dict:fetch(fitness,Dict),
    {noreply, State#state{counters = createCounter(Best,NewPopulation)}}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, State) ->
    closeFiles(State#state.fds).

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec createCounter(float(),integer()) -> dict().
createCounter(Fitness,Population) ->
    lists:foldl(fun({Key,Val},Dict) ->
                        dict:store(Key,Val,Dict)
                end,dict:new(),[{reproduction,0},
                                {death,0},
                                {emigration,0},
                                {immigration,0},
                                {fight,0},
                                {population,Population},
                                {fitness,Fitness}]).

%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow niesekwencyjnych
-spec prepareParDictionary([pid()], dict(), string()) -> dict().
prepareParDictionary([], Dict, Path) ->
    createFDs(Path, Dict, ?GLOBAL_STATS);

prepareParDictionary([H|T], Dict, Path) ->
    IslandPath = case Path of
                     standard_io ->
                         standard_io;
                     _ ->
                         NewPath = filename:join([Path, "island" ++ integer_to_list(length(T) + 1)]),
                         file:make_dir(NewPath),
                         NewPath
                 end,
    NewDict = dict:store(H, createFDs(IslandPath, dict:new(), ?LOCAL_STATS), Dict), % Key = pid(), Value = dictionary of file descriptors
    prepareParDictionary(T, NewDict, Path).

%% @doc Tworzy pliki tekstowe do zapisu i zwraca dict() z deskryptorami.
-spec createFDs(string(), dict(), [atom()]) -> FDs :: dict().
createFDs(standard_io, InitDict, Files) ->
    lists:foldl(fun(Atom, Dict) ->
                        dict:store(Atom, standard_io, Dict)
                end, InitDict,
                Files);

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
    file:write(FD, io_lib:fwrite("~p ~p ~p\n", [Statistic, Key, Value])).

%% @doc Dokonuje buforowanego zapisu do pliku globalnej statystyki. W argumencie podany glowny slownik, nazwa statystyki i wartosc do wpisania.
-spec logGlobal(dict(), atom(), term()) -> ok.
logGlobal(Dictionary, Stat, Value) ->
    FD = dict:fetch(Stat, Dictionary),
    file:write(FD, io_lib:fwrite("~p ~p\n", [Stat,Value])).

%% @doc Zamyka pliki podane w argumencie
-spec closeFiles(dict()) -> any().
closeFiles(Dict) ->
    [case X of
         {Id, FD} when is_atom(Id) -> file:close(FD);
         {_Id, D} -> [file:close(FD) || {_Stat, FD} <- dict:to_list(D)]
     end || X <- dict:to_list(Dict)].

-spec addCounters([tuple()], [tuple()]) -> [tuple()].
addCounters([],Other) ->
    Other;

addCounters(Other,[]) ->
    Other;

addCounters(L1,L2) ->
    addCounters(L1,L2,[]).

-spec addCounters([tuple()], [tuple()], [tuple()]) -> [tuple()].
addCounters([],_Other,Result) ->
    Result;

addCounters([{Name,Val1}|T],Other,Result) ->
    {Name,Val2} = lists:keyfind(Name,1,Other),
    addCounters(T,Other,[{Name,Val1 + Val2}|Result]).