%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer logger odpowiada za pisanie statystyk do plikow
-module(logger).
-behaviour(gen_server).

%% API
-export([start_link/2, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(atom(),string()) -> {ok,pid()}.
start_link(Model,Path) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Model,Path], []).

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

handle_cast({Stat,Pid,Value}, Dict) when is_float(Value) ->
  log(Dict,Pid,Stat,Value),
  {noreply, Dict};
handle_cast({Stat,_Pid,Values}, Dict) when is_list(Values) ->
  logList(Stat,1,Values,Dict),
  {noreply, Dict};
handle_cast(close, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec prepareParDictionary([pid()],dict(),string()) -> dict().
%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow niesekwencyjnych
prepareParDictionary([],Dict,_) ->
  Dict;
prepareParDictionary([H|T],Dict,Path) ->
  IslandPath = filename:join([Path,"island" ++ integer_to_list(length(T)+1)]),
  file:make_dir(IslandPath),
  NewDict = dict:store(H,createFDs(IslandPath),Dict), % Key = pid(), Value = dictionary of file descriptors
  prepareParDictionary(T,NewDict,Path).

-spec prepareSeqDictionary(non_neg_integer(),dict(),string()) -> dict().
%% @doc Tworzy duzy slownik z mniejszymi slownikami deskryptorow dla kazdej z wysp dla modelow sekwencyjnych
prepareSeqDictionary(0,Dict,_) ->
  Dict;
prepareSeqDictionary(IslandNr,Dict,Path) ->
  IslandPath = filename:join([Path,"island" ++ integer_to_list(IslandNr)]),
  file:make_dir(IslandPath),
  NewDict = dict:store(IslandNr,createFDs(IslandPath),Dict), % Key = IslandNumber, Value = dictionary of file descriptors
  prepareSeqDictionary(IslandNr-1,NewDict,Path).

-spec createFDs(string()) -> FDs :: dict().
%% @doc Funkcja tworzy pliki tekstowe do zapisu i zwraca dict() z deskryptorami.
createFDs(Path) ->
  lists:foldl(fun(Atom,Dict) ->
    Filename = atom_to_list(Atom) ++ ".txt",
    {ok,Descriptor} = file:open(filename:join([Path,Filename]),[append,delayed_write,raw]),
    dict:store(Atom,Descriptor,Dict)
  end,dict:new(),
  [fitness,population]).

logList(_,_,[],_) ->
  ok;
logList(Stat,Index,[H|T],Dict) ->
  log(Dict,Index,Stat,H),
  logList(Stat,Index+1,T,Dict).

-spec log(dict(),term(),atom(),term()) -> ok.
%% @doc Funkcja dokonuje buforowanego zapisu do pliku. W argumencie podana statystyka (np. fitness), klucz, wartosc do zapisania i duzy slownik.
log(Dictionary,Key,Statistic,Value) ->
  FDs = dict:fetch(Key,Dictionary),
  FD = dict:fetch(Statistic,FDs),
  file:write(FD,io_lib:fwrite("~p\n",[Value])).




