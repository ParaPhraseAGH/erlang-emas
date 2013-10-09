%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer logger odpowiada za pisanie statystyk do plikow
-module(logger).
-behaviour(gen_server).

%% API
-export([start_link/2, logLocalStats/3, logGlobalStats/1, agregateGlobalStats/1, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(atom(),string()) -> {ok,pid()}.
start_link(Model,Path) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Model,Path], []).

-spec logLocalStats(sequential | parallel,atom(),term()) -> ok.
%% @doc Funkcja loguje statystyki specyficzne dla danej wyspy (np. fitness, population).
%% Pierwsza zmienna informuje o trybie zapisu: parallel dla modelu concurrent i hybrid, a sequential dla sekwencyjnych.
logLocalStats(Mode,Stat,Value) ->
  gen_server:cast(whereis(?MODULE),{Mode,Stat,self(),Value}).

-spec logGlobalStats(tuple()) -> ok.
%% @doc Funkcja zapisuje globalne statystyki (deaths,fights etc.) do plikow.
logGlobalStats(Counter) ->
  gen_server:cast(whereis(?MODULE),{counter,Counter}).

agregateGlobalStats(Counter) ->
  gen_server:cast(whereis(?MODULE),{agregate,self(),Counter}).

-spec close() -> ok.
close() ->
  gen_server:cast(whereis(?MODULE),close).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Model,Path]) ->
  Dict = case Model of
    {sequential,IslandsNr} ->
      prepareSeqDictionary(IslandsNr,dict:new(),Path);
    {parallel,Pids} when is_list(Pids)->
      prepareParDictionary(Pids,dict:new(),Path)
  end,
  {ok, Dict}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({parallel,Stat,Pid,Value}, Dict) ->
  logLocal(Dict,Pid,Stat,Value),
  {noreply, Dict};
handle_cast({sequential,Stat,_Pid,Values}, Dict) ->
  logList(Stat,1,Values,Dict),
  {noreply, Dict};
handle_cast({counter,{Deaths,Fights,Reproductions,Migrations}}, Dict) ->
  logGlobal(Dict,death,Deaths),
  logGlobal(Dict,fight,Fights),
  logGlobal(Dict,reproduction,Reproductions),
  logGlobal(Dict,migration,Migrations),
  {noreply, Dict};
handle_cast(close, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, Dict) ->
  closeFiles(Dict).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec prepareParDictionary([pid()],dict(),string()) -> dict().
%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow niesekwencyjnych
prepareParDictionary([],Dict,Path) ->
  createFDs(Path,Dict,[death,fight,reproduction,migration]);
prepareParDictionary([H|T],Dict,Path) ->
  IslandPath = filename:join([Path,"island" ++ integer_to_list(length(T)+1)]),
  file:make_dir(IslandPath),
  NewDict = dict:store(H,createFDs(IslandPath,dict:new(),[fitness,population]),Dict), % Key = pid(), Value = dictionary of file descriptors
  prepareParDictionary(T,NewDict,Path).

-spec prepareSeqDictionary(non_neg_integer(),dict(),string()) -> dict().
%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow sekwencyjnych
prepareSeqDictionary(0,Dict,Path) ->
  createFDs(Path,Dict,[death,fight,reproduction,migration]);
prepareSeqDictionary(IslandNr,Dict,Path) ->
  IslandPath = filename:join([Path,"island" ++ integer_to_list(IslandNr)]),
  file:make_dir(IslandPath),
  NewDict = dict:store(IslandNr,createFDs(IslandPath,dict:new(),[fitness,population]),Dict), % Key = IslandNumber, Value = dictionary of file descriptors
  prepareSeqDictionary(IslandNr-1,NewDict,Path).

-spec createFDs(string(),dict(),[atom()]) -> FDs :: dict().
%% @doc Funkcja tworzy pliki tekstowe do zapisu i zwraca dict() z deskryptorami.
createFDs(Path,InitDict,Files) ->
  lists:foldl(fun(Atom,Dict) ->
    Filename = atom_to_list(Atom) ++ ".txt",
    {ok,Descriptor} = file:open(filename:join([Path,Filename]),[append,delayed_write,raw]),
    dict:store(Atom,Descriptor,Dict)
  end,InitDict,
  Files).

-spec logList(atom(),pos_integer(),[term()],dict()) -> ok.
logList(_,_,[],_) ->
  ok;
logList(Stat,Index,[H|T],Dict) ->
  logLocal(Dict,Index,Stat,H),
  logList(Stat,Index+1,T,Dict).

-spec logLocal(dict(),term(),atom(),term()) -> ok.
%% @doc Funkcja dokonuje buforowanego zapisu do pliku lokalnej statystyki. W argumencie podany glowny slownik, klucz, nazwa statystyki i wartosc do wpisania.
logLocal(Dictionary,Key,Statistic,Value) ->
  FDs = dict:fetch(Key,Dictionary),
  FD = dict:fetch(Statistic,FDs),
  file:write(FD,io_lib:fwrite("~p\n",[Value])).

-spec logGlobal(dict(),atom(),term()) -> ok.
%% @doc Funkcja dokonuje buforowanego zapisu do pliku globalnej statystyki. W argumencie podany glowny slownik, nazwa statystyki i wartosc do wpisania.
logGlobal(Dictionary,Stat,Value) ->
  FD = dict:fetch(Stat,Dictionary),
  file:write(FD,io_lib:fwrite("~p\n",[Value])).

-spec closeFiles(dict()) -> any().
%% @doc Funkcja zamykająca pliki podane w argumencie
closeFiles(Dict) ->
  [case X of
     {Id,FD} when is_atom(Id) -> file:close(FD);
     {_Id,D} -> [file:close(FD) || {_Stat,FD} <- dict:to_list(D)]
   end || X <- dict:to_list(Dict)].







