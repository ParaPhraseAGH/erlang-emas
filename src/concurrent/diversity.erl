-module(diversity).
-author("jstypka").

-behaviour(gen_server).

%% API
-export([start_link/1, newAgent/2, report/3, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

-spec start_link(pid()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Supervisor) ->
    gen_server:start_link(?MODULE, [Supervisor], []).

-spec newAgent(pid(),{pid(),agent()}) -> ok.
newAgent(Pid,AgentPkg) ->
    gen_server:cast(Pid,{newAgent,AgentPkg}).

-spec report(pid(),atom(),term()) -> ok.
report(Pid,Arena,Value) ->
    gen_server:cast(Pid,{report,Arena,Value}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-record(state, {supervisor :: pid(),
                agents = gb_trees:empty() :: gb_tree(),
                reports = orddict:new() :: [{term(),term()}],
                db4b = [] :: list()}).
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
handle_cast({report,Arena,Value},State) ->
    Dict = State#state.reports,
    error = orddict:find(Arena,Dict), % debug - tu wyskakuje error!
    NewDict = orddict:store(Arena,Value,Dict),
    case orddict:size(NewDict) of
        3 ->
            %%             {Agents,Db4b} = logStats(NewDict,State),
            %%             {noreply,State#state{reports = orddict:new(), agents = Agents, db4b = Db4b}};
            conc_logger:log(State#state.supervisor,diversity,Value), %todo Value jest przykladowe
            {noreply,State#state{reports = orddict:new()}};
        _ ->
            {noreply,State#state{reports = NewDict}}
    end;

handle_cast({newAgent,{Pid,Agent}},State) ->
    NewAgents = gb_trees:insert(Pid,Agent,State#state.agents),
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

-spec logStats([{term(),term()}],state()) -> {gb_tree(),list()}.
logStats(Dict,State) ->
    Deaths = orddict:fetch(death,Dict),
    {Emigrations,Immigrations} = orddict:fetch(migration,Dict),
    {_Best,Reproductions} = orddict:fetch(reproduction,Dict),
    Add1 = Reproductions ++ Immigrations,
    Del1 = Deaths ++ Emigrations,
    Db4b = State#state.db4b -- [Pid || {Pid,_} <- Add1],
    Add2 = lists:foldl(fun(Pid,List) ->
                               lists:keydelete(Pid,1,List)
                       end,Add1,State#state.db4b),
    Add3 = lists:foldl(fun(Pid,List) ->
                               lists:keydelete(Pid,1,List)
                       end,Add2,Del1),
    Del3 = Del1 -- [X || {X,_} <- Add2],
    %%     case lists:sort([P || {P,_} <- Add3]) == lists:usort([P || {P,_} <- Add3]) of
    %%         true ->
    %%             nothing;
    %%         false ->
    %%             error("DUPLICATE!")
    %%     end,
    %%     true = lists:all(fun({Pid,_}) ->
    %%                              not gb_trees:is_defined(Pid,State#state.agents)
    %%                      end,Add3),
    Population1 = lists:foldl(fun({Key,Val},Tree) ->
                                      gb_trees:insert(Key,Val,Tree)
                              end, State#state.agents, Add3),
    {NewAgents,NewDb4b} = lists:foldl(fun(Pid,{Tree,TMPdb4b}) ->
                                              case gb_trees:is_defined(Pid,Tree) of
                                                  false ->
                                                      {Tree,[Pid|TMPdb4b]};
                                                  true ->
                                                      {gb_trees:delete(Pid,Tree),TMPdb4b}
                                              end
                                      end, {Population1,Db4b}, Del3),
    {SumVar,MinVar,VarVar} = misc_util:diversity([Val || {_Key,Val} <- gb_trees:to_list(NewAgents)]),
    io:format("diversity~n"),
    %%     logger:logLocalStats(parallel,stddevsum,SumVar),
    %%     logger:logLocalStats(parallel,stddevmin,MinVar),
    %%     logger:logLocalStats(parallel,stddevvar,VarVar),
    {gb_trees:balance(NewAgents),NewDb4b}.
