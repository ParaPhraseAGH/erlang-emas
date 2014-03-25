-module(cemetery).
-behaviour(gen_server).

-define(TIMEOUT,5000).

%% API
-export([start_link/2, start/1, cast/1, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {supervisor :: pid(),
                diversity :: pid(),
                deaths = [] :: [pid()],
                lastLog :: erlang:timestamp()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(),pid()) -> {ok, Pid :: pid()} |
                                 {error, Reason :: term()} |
                                 ignore.
start_link(Supervisor,Diversity) ->
    gen_server:start_link(?MODULE, [Supervisor,Diversity], []).

-spec start(pid()) -> {ok,pid()}.
start(Supervisor) ->
    gen_server:start(?MODULE, [Supervisor], []).

-spec cast(pid()) -> ok.
cast(Pid) ->
    gen_server:cast(Pid,{died,self()}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init(Args :: term()) ->
                  {ok, State :: #state{}} |
                  {ok, State :: #state{}, timeout() | hibernate} |
                  {stop, Reason :: term()} |
                  ignore.
init([Supervisor,Diversity]) ->
    misc_util:seedRandom(),
    {ok, #state{supervisor = Supervisor, lastLog = os:timestamp(), diversity = Diversity}, ?TIMEOUT}.


-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
                         {reply, Reply :: term(), NewState :: #state{}} |
                         {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                         {noreply, NewState :: #state{}} |
                         {noreply, NewState :: #state{}, timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                         {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(Request :: term(), State :: #state{}) ->
                         {noreply, NewState :: #state{}} |
                         {noreply, NewState :: #state{}, timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(close, State) ->
    {stop,normal,State};
handle_cast({died,Pid}, State) ->
    Deaths = [Pid|State#state.deaths],
    case misc_util:logNow(State#state.lastLog) of
        {yes,NewLog} ->
            diversity:report(State#state.diversity,death,Deaths),
            conc_logger:log(State#state.supervisor,death,length(Deaths)),
            {noreply,State#state{lastLog = NewLog,
                                 deaths = []},?TIMEOUT};
        notyet ->
            {noreply,State#state{deaths = Deaths},?TIMEOUT}
    end.


-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
                         {noreply, NewState :: #state{}} |
                         {noreply, NewState :: #state{}, timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.


-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
                         {ok, NewState :: #state{}} |
                         {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
