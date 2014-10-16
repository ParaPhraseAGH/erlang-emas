-module(mas_conc_arena).
-behaviour(gen_server).

%% API
-export([start_link/4, giveArenas/2, call/2, close/1]).

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
                counter = 0 :: non_neg_integer(),
                sim_params :: sim_params(),
                config :: config()}).

-define(CLOSING_TIMEOUT, 2000).
-define(AGENT_THRESHOLD, 2). %% TODO this should be user-configurable and use case dependent

-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(), atom(), sim_params(), config()) -> pid().
start_link(Supervisor, migration, _SP, Cf) ->
    {ok, Pid} = mas_conc_port:start_link(Supervisor, Cf),
    Pid;

start_link(Supervisor, Interaction, SP, Cf) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Supervisor, Interaction, SP, Cf], []),
    Pid.

%% @doc Sends a request with given agent to this arena
-spec call(pid(),agent()) -> agent() | close.
call(Pid, Agent) ->
    gen_server:call(Pid, {interact, Agent}, infinity).

-spec giveArenas(pid(),dict:dict()) -> ok.
giveArenas(Pid, Arenas) ->
    gen_server:call(Pid, {arenas, Arenas}, infinity).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec(init(Args :: term()) -> {ok, State :: #state{}} |
                              {ok, State :: #state{}, timeout() | hibernate} |
                              {stop, Reason :: term()} | ignore).
init([Supervisor, Interaction, SP, Cf]) ->
    mas_misc_util:seed_random(),
    Env = Cf#config.agent_env,
    Funstats = Env:stats(),
    {ok, #state{supervisor = Supervisor,
                lastLog = os:timestamp(),
                interaction = Interaction,
                funstats = Funstats,
                sim_params = SP,
                config = Cf}, Cf#config.arena_timeout}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).

handle_call({interact, _Agent}, _From, cleaning) ->
    {reply, the_end, cleaning, ?CLOSING_TIMEOUT};

handle_call({interact, Agent}, From, St = #state{sim_params = SP, config = Cf}) ->
    Waitlist = [Agent|St#state.waitlist],
    Froms = [From|St#state.agentFroms],
    case length(Waitlist)  of
        ?AGENT_THRESHOLD ->
            NewAgents = mas_misc_util:meeting_proxy({St#state.interaction, Waitlist}, concurrent, SP, Cf),
            respond(NewAgents, Froms, St#state.arenas, SP, Cf),

            NewCounter = St#state.counter + length(Waitlist), % tu blad?!
            NewFunstats = mas_misc_util:count_funstats(NewAgents, St#state.funstats),

            case mas_misc_util:log_now(St#state.lastLog, Cf) of
                {yes, NewLog} ->
                    mas_logger:log_countstat(St#state.supervisor, St#state.interaction, NewCounter),
                    [mas_logger:log_funstat(St#state.supervisor, StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- NewFunstats],
                    {noreply,St#state{waitlist = [],
                                         agentFroms = [],
                                         lastLog = NewLog,
                                         funstats = NewFunstats,
                                         counter = 0}, Cf#config.arena_timeout};
                notyet ->
                    {noreply,St#state{waitlist = [],
                                         agentFroms = [],
                                         funstats = NewFunstats,
                                         counter = NewCounter}, Cf#config.arena_timeout}
            end;
        _ ->
            {noreply, St#state{agentFroms = Froms, waitlist = Waitlist}, Cf#config.arena_timeout}
    end;

handle_call({arenas, Arenas}, _From, St = #state{config = Cf}) ->
    {reply, ok, St#state{arenas = Arenas}, Cf#config.arena_timeout}.

-spec(handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} |
                                                           {noreply, NewState :: #state{}, timeout() | hibernate} |
                                                           {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(close, _State) ->
    {noreply, cleaning, ?CLOSING_TIMEOUT}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) -> {noreply, NewState :: #state{}} |
                                                                    {noreply, NewState :: #state{}, timeout() | hibernate} |
                                                                    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, cleaning) ->
    {stop, normal, cleaning};

handle_info(timeout, State) ->
    {stop, timeout, State}.

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

-spec respond([agent()], {pid(), term()}, dict:dict(), sim_params(), config()) -> list().
respond(Agents, Froms, Arenas, SP, Cf) when length(Agents) >= length(Froms) ->
    [gen_server:reply(From, Agent) || {From, Agent} <- mas_misc_util:shortest_zip(Froms, Agents)],
    [spawn(mas_conc_agent, start, [Agent, Arenas, SP, Cf]) || Agent <- lists:nthtail(length(Froms), Agents)];

respond(Agents, Froms, _Arenas, _SimParams, _Config) when length(Agents) =< length(Froms) ->
    [gen_server:reply(From, Agent) || {From, Agent} <- mas_misc_util:shortest_zip(Froms, Agents)],
    [gen_server:reply(From, close) || From <- lists:nthtail(length(Agents), Froms)].
