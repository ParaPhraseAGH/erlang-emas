-module(arena).
-behaviour(gen_server).

%% API
-export([start_link/2, giveArenas/2, call/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include ("mas.hrl").

-record(state, {supervisor :: pid(),
                waitlist = [] :: list(),
                agentFroms = [] ::[pid()],
                funstats :: [funstat()],
                arenas :: dict:dict(),
                interaction :: atom(),
                lastLog :: erlang:timestamp(),
                counter = 0 :: non_neg_integer()}).

-define(CLOSING_TIMEOUT, 2000).
-define(AGENT_THRESHOLD, 2). %% TODO zmienna powinna byc konfigurowana przez usera i na dodatek zalezna od interakcji

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(),atom()) -> pid().
start_link(Supervisor,migration) ->
    {ok,Pid} = port:start_link(Supervisor),
    Pid;

start_link(Supervisor,Interaction) ->
    {ok,Pid} = gen_server:start_link(?MODULE, [Supervisor,Interaction], []),
    Pid.

%% @doc Funkcja wysylajaca zgloszenie agenta do areny.
-spec call(pid(),agent()) -> agent() | close.
call(Pid,Agent) ->
    gen_server:call(Pid,{interact,Agent},infinity).

-spec giveArenas(pid(),dict:dict()) -> ok.
giveArenas(Pid,Arenas) ->
    gen_server:call(Pid,{arenas,Arenas},infinity).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec(init(Args :: term()) -> {ok, State :: #state{}} |
                              {ok, State :: #state{}, timeout() | hibernate} |
                              {stop, Reason :: term()} | ignore).
init([Supervisor,Interaction]) ->
    misc_util:seedRandom(),
    Env = config:agent_env(),
    Funstats = Env:stats(),
    {ok, #state{supervisor = Supervisor,
                lastLog = os:timestamp(),
                interaction = Interaction,
                funstats = Funstats},config:arenaTimeout()}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).

handle_call({interact,_Agent}, _From, cleaning) ->
    {reply,close,cleaning,?CLOSING_TIMEOUT};

handle_call({interact,Agent}, From, State) ->
    Waitlist = [Agent|State#state.waitlist],
    Froms = [From|State#state.agentFroms],
    case length(Waitlist)  of
        ?AGENT_THRESHOLD ->
            NewAgents = misc_util:meeting_proxy({State#state.interaction,Waitlist},concurrent),
            respond(NewAgents,Froms,State#state.arenas),

            NewCounter = State#state.counter + length(Waitlist), % tu blad?!
            NewFunstats = misc_util:count_funstats(NewAgents, State#state.funstats),

            case misc_util:logNow(State#state.lastLog) of
                {yes,NewLog} ->
                    %%                     conc_logger:log(State#state.supervisor,State#state.interaction,NewCounter),
                    logger:log_countstat(State#state.supervisor, State#state.interaction, NewCounter),
                    [logger:log_funstat(State#state.supervisor, StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- NewFunstats],
                    {noreply,State#state{waitlist = [],
                                         agentFroms = [],
                                         lastLog = NewLog,
                                         funstats = NewFunstats,
                                         counter = 0},config:arenaTimeout()};
                notyet ->
                    {noreply,State#state{waitlist = [],
                                         agentFroms = [],
                                         funstats = NewFunstats,
                                         counter = NewCounter},config:arenaTimeout()}
            end;
        _ ->
            {noreply,State#state{agentFroms = Froms, waitlist = Waitlist},config:arenaTimeout()}
    end;

handle_call({arenas,Arenas}, _From, State) ->
    {reply,ok,State#state{arenas = Arenas},config:arenaTimeout()}.

-spec(handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} |
                                                           {noreply, NewState :: #state{}, timeout() | hibernate} |
                                                           {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(close, _State) ->
    {noreply, cleaning, ?CLOSING_TIMEOUT}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) -> {noreply, NewState :: #state{}} |
                                                                    {noreply, NewState :: #state{}, timeout() | hibernate} |
                                                                    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout,cleaning) ->
    {stop,normal,cleaning};

handle_info(timeout,State) ->
    {stop,timeout,State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec respond([agent()], {pid(), term()}, dict:dict()) -> list().
respond(Agents, Froms, Arenas) when length(Agents) >= length(Froms) ->
    [gen_server:reply(From,Agent) || {From,Agent} <- misc_util:shortestZip(Froms,Agents)],
    [spawn(agent,start,[Agent,Arenas]) || Agent <- lists:nthtail(length(Froms),Agents)];

respond(Agents, Froms, _Arenas) when length(Agents) =< length(Froms) ->
    [gen_server:reply(From,Agent) || {From,Agent} <- misc_util:shortestZip(Froms,Agents)],
    [gen_server:reply(From,close) || From <- lists:nthtail(length(Agents),Froms)].
