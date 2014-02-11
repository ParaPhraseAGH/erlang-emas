-module(diversity).
-author("jstypka").

-behaviour(gen_server).

%% API
-export([start_link/1, initPopulation/2, report/3, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

-spec start_link(pid()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Supervisor) ->
    gen_server:start_link(?MODULE, [Supervisor], []).

-spec initPopulation(pid(),[{pid(),agent()}]) -> ok.
initPopulation(Pid,Agents) ->
    case config:monitorDiversity() of
        true -> gen_server:cast(Pid,{initPopulation,Agents});
        false -> ok
    end.

-spec report(pid(),atom(),term()) -> ok.
report(Pid,Arena,Value) ->
    case config:monitorDiversity() of
        true -> gen_server:cast(Pid,{report,Arena,Value});
        false -> ok
    end.

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {supervisor :: pid(),
                agents = gb_trees:empty() :: gb_tree(),
                reports = dict:new() :: dict(),
                toDelete = [] :: list()}).
-type state() :: #state{}.

-spec init(Args :: term()) -> {ok, State :: #state{}} |
                              {ok, State :: #state{}, timeout() | hibernate} |
                              {stop, Reason :: term()} | ignore.
init([Supervisor]) ->
    {ok, #state{supervisor = Supervisor}}.


-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
                         {reply, Reply :: term(), NewState :: #state{}} |
                         {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                         {noreply, NewState :: #state{}} |
                         {noreply, NewState :: #state{}, timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                         {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} |
                                                           {noreply, NewState :: #state{}, timeout() | hibernate} |
                                                           {stop, Reason :: term(), NewState :: #state{}}.
handle_cast({report,Arena,Value},State = #state{reports = Dict}) ->
    NewDict = case dict:is_key(Arena,Dict) of
                  true ->
                      io:format("Report overwrite in diversity ~p~n",[self()]), % mozna zakomentowac dla wysokich parametrow programu
                      dict:update(Arena,fun(OldVal) -> Value ++ OldVal end,Dict);
                  false ->
                      dict:store(Arena,Value,Dict)
              end,
    case dict:size(NewDict) of
        4 ->
            {Agents,ToDelete} = logStats(NewDict,State),
            {noreply,State#state{reports = dict:new(), agents = Agents, toDelete = ToDelete}};
        _ ->
            {noreply,State#state{reports = NewDict}}
    end;

handle_cast({initPopulation,Agents},State) ->
    NewAgents = lists:foldl(fun({Pid,Agent},Tree) ->
                                    gb_trees:insert(Pid,Agent,Tree)
                            end,State#state.agents,Agents),
    {noreply,State#state{agents = NewAgents}};

handle_cast(close,State) ->
    {stop,normal,State}.


-spec handle_info(Info :: timeout() | term(), State :: #state{}) -> {noreply, NewState :: #state{}} |
                                                                    {noreply, NewState :: #state{}, timeout() | hibernate} |
                                                                    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.


-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
                         {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec logStats(dict(),state()) -> {gb_tree(),list()}.
logStats(Dict,State) ->
    Deaths = dict:fetch(death,Dict),
    Emigrations = dict:fetch(emigration,Dict),
    Immigrations = dict:fetch(immigration,Dict),
    Reproductions = dict:fetch(reproduction,Dict),

    FirstAdd = lists:foldl(fun({Pid,Agent},Tree) ->
                                   gb_trees:insert(Pid,Agent,Tree)
                           end,State#state.agents,Reproductions),

    {ThenDelete,MoreToDelete} = tryToDeleteFromTree({FirstAdd,State#state.toDelete}, Deaths),

    ClearedImmigrants = lists:foldl(fun(Pid,List) ->
                                            lists:keydelete(Pid,1,List)
                                    end,Immigrations,Emigrations),
    ClearedEmigrants = Emigrations -- [Pid || {Pid,_} <- Immigrations],

    AddImmigrants = lists:foldl(fun({Pid,Agent},Tree) ->
                                        gb_trees:insert(Pid,Agent,Tree)
                                end,ThenDelete,ClearedImmigrants),
    {DeleteEmigrants, NewToDelete} = tryToDeleteFromTree({AddImmigrants,[]}, MoreToDelete ++ ClearedEmigrants),

    Variances = misc_util:diversity([Val || {_Key,Val} <- gb_trees:to_list(DeleteEmigrants)]),
    conc_logger:log(State#state.supervisor,diversity,Variances),
    {DeleteEmigrants,NewToDelete}.

-spec tryToDeleteFromTree({gb_tree(),[pid()]},[pid()]) -> {gb_tree(),[pid()]}.
tryToDeleteFromTree(Acc0,List) ->
    lists:foldl(fun(Pid,{Tree,TMPToDelete}) ->
                        case catch gb_trees:delete(Pid,Tree) of
                            {'EXIT',_} ->
                                {Tree,[Pid|TMPToDelete]};
                            NewTree ->
                                {NewTree,TMPToDelete}
                        end
                end, Acc0, List).