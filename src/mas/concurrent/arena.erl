-module(arena).
-behaviour(gen_server).

%% API
-export([start_link/2, giveArenas/2, call/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {supervisor :: pid(),
                waitlist = [] :: list(),
                agentFroms = [] ::[pid()],
                arenas :: dict:dict(),
                interaction :: atom(),
                lastLog :: erlang:timestamp(),
                counter = 0 :: non_neg_integer()}).

-define(CLOSING_TIMEOUT,2000).
-define(AGENT_THRESHOLD,2). %% TODO zmienna powinna byc konfigurowana przez usera i na dodatek zalezna od interakcji

-type agent() :: term(). %% TODO moglby go user definiowac
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
-spec call(pid(),agent()) -> Energy :: integer().
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
    {ok, #state{supervisor = Supervisor, lastLog = os:timestamp(), interaction = Interaction},config:arenaTimeout()}.


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
            NewCounter = State#state.counter + length(Waitlist), % tu blad?!
            NewAgents = misc_util:meeting_proxy({State#state.interaction,Waitlist},concurrent),
            respond(NewAgents,Froms,State#state.arenas),
            case misc_util:logNow(State#state.lastLog) of
                {yes,NewLog} ->
                    conc_logger:log(State#state.supervisor,State#state.interaction,NewCounter),
                    {noreply,State#state{waitlist = [],
                                         agentFroms = [],
                                         lastLog = NewLog,
                                         counter = 0},config:arenaTimeout()};
                notyet ->
                    {noreply,State#state{waitlist = [],
                                         agentFroms = [],
                                         counter = NewCounter},config:arenaTimeout()}
            end;
        _ ->
            {noreply,State#state{agentFroms = Froms, waitlist = Waitlist},config:arenaTimeout()}
    end;

handle_call({arenas,Arenas}, _From, State) ->
    {reply,ok,State#state{arenas = Arenas},config:arenaTimeout()}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(close, _State) ->
    {noreply, cleaning, ?CLOSING_TIMEOUT}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
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

respond([],Froms,_Arenas) ->
    [gen_server:reply(From,close) || From <- Froms];

respond(Agents,Froms,Arenas) when length(Agents) >= length(Froms) ->
    [gen_server:reply(From,Agent) || {From,Agent} <- misc_util:shortestZip(Froms,Agents)],
%%     case lists:nthtail(length(Froms),Agents) of
%%         [] -> nothing;
%%         L ->
%%             case random:uniform() < 0.001 of
%%                 true ->
%%                     io:format("~p ~p~n",[self(),lists:max([Fit || {_,Fit,_} <- L ])]);
%%                 false ->
%%                     nothing
%%             end
%%     end,
    [spawn(agent,start,[Agent,Arenas]) || Agent <- lists:nthtail(length(Froms),Agents)].