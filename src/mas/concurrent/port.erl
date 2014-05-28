%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny migracji (portu).
-module(port).
-behaviour(gen_server).

-define(TIMEOUT,10000).

%% API
-export([start_link/1, giveArenas/2, immigrate/2, emigrate/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid()) -> {ok,pid()}.
start_link(Supervisor) ->
    gen_server:start_link(?MODULE, [Supervisor], []).

-spec giveArenas(pid(),dict:dict()) -> ok.
giveArenas(Pid,Arenas) ->
    gen_server:call(Pid,{arenas,Arenas}).

-spec immigrate(pid(),tuple()) -> ok.
immigrate(Pid,AgentInfo) ->
    gen_server:cast(Pid,{immigrant,AgentInfo}).

%% @doc Funkcja wysylajaca zgloszenie agenta do portu.
-spec emigrate(pid(),agent()) -> [pid()].
emigrate(Pid,Agent) ->
    gen_server:call(Pid,{emigrate,Agent},infinity).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {mySupervisor :: pid(),
                arenas :: dict:dict(),
                funstats :: [funstat()],
                emigrants = [] :: [pid()],
                immigrants = [] :: [{pid(),agent()}],
                lastLog :: erlang:timestamp()}).
-type state() :: #state{} | cleaning.


-spec init([pid()]) -> {ok,state()} |
                       {ok,state(),non_neg_integer()}.
init([Supervisor]) ->
    misc_util:seedRandom(),
    timer:send_after(config:writeInterval(),timer),
    Env = config:agent_env(),
    Funstats = Env:stats(),
    {ok, #state{mySupervisor = Supervisor,
                lastLog = os:timestamp(),
                funstats = Funstats}}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call({arenas,Arenas}, _From, State) ->
    topology:helloPort(),
    {reply,ok,State#state{arenas = Arenas}};

handle_call({emigrate,_Agent},{Pid,_},cleaning) ->
    exit(Pid,finished),
    {noreply,cleaning,?TIMEOUT};

handle_call({emigrate,Agent}, From, State) ->
    {HisPid, _} = From,
    {Emigrants,Immigrants,LastLog} = check(State),
    topology:emigrant({Agent,From}),
    {noreply,State#state{emigrants = [HisPid|Emigrants], immigrants = Immigrants, lastLog = LastLog}}.


-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.

handle_cast({immigrant,{_Agent,{Pid,_}}}, cleaning) ->
    exit(Pid,finished),
    {noreply,cleaning,?TIMEOUT};

handle_cast({immigrant,{Agent,From}}, State) ->
    gen_server:reply(From,State#state.arenas),
    {Emigrants,Immigrants,LastLog} = check(State),
    {HisPid, _} = From,
    {noreply,State#state{immigrants = [{HisPid,Agent}|Immigrants], emigrants = Emigrants, lastLog = LastLog}};

handle_cast(close, _State) ->
    {noreply,cleaning,?TIMEOUT}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,cleaning) ->
    {stop,normal,cleaning};

handle_info(timer,cleaning) ->
    {noreply,cleaning,config:writeInterval()/2};

handle_info(timer,State) ->
    {Emigrants, Immigrants, LastLog} = check(State),
    {noreply,State#state{emigrants = Emigrants,
                         immigrants = Immigrants,
                         lastLog = LastLog}}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check(state()) -> {[pid()],[{pid(),agent()}],erlang:timestamp()}.
check(#state{emigrants = Emigrants,
             immigrants = Immigrants,
             funstats = Funstats,
             lastLog = LastLog,
             mySupervisor = Supervisor}) ->
    case misc_util:logNow(LastLog) of
        {yes,NewLog} ->
            logger:log_countstat(Supervisor, migration, length(Emigrants)),
            [logger:log_funstat(Supervisor, StatName, Val) || {StatName, _MapFun, _ReduceFun, Val} <- Funstats],
            %%             conc_logger:log(State#state.mySupervisor,migration,{length(Emigrants),length(Immigrants)}),
            timer:send_after(config:writeInterval(),timer),
            {[],[],NewLog};
        notyet ->
            {Emigrants,Immigrants,LastLog}
    end.