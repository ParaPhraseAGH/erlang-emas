%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny migracji (portu).
-module(port).
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, immigrate/2, call/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid(),[pid()]) -> {ok,pid()}.
start_link(Supervisor,Arenas) ->
    gen_server:start_link(?MODULE, [Supervisor,Arenas], []).

-spec start(pid(),[pid()]) -> {ok,pid()}.
start(Supervisor,Arenas) ->
    gen_server:start(?MODULE, [Supervisor,Arenas], []).

-spec immigrate(pid(),tuple()) -> ok.
immigrate(Pid,AgentInfo) ->
    gen_server:cast(Pid,{immigrant,AgentInfo}).

%% @doc Funkcja wysylajaca zgloszenie agenta do portu.
-spec call(pid(),agent()) -> [pid()].
call(Pid,Agent) ->
    gen_server:call(Pid,{emigrate,Agent}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {mySupervisor :: pid(),
                arenas = [] :: [pid()],
                emigrants = [] :: [pid()],
                immigrants = [] :: [{pid(),agent()}],
                lastLog :: erlang:timestamp()}).
-type state() :: #state{} | cleaning.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([Supervisor,{Ring,Bar,Cemetery}]) ->
    misc_util:seedRandom(),
    topology_conc:helloPort(),
    timer:send_interval(config:writeInterval(),timer),
    {ok, #state{mySupervisor = Supervisor, arenas = [Ring,Bar,self(),Cemetery],  lastLog = os:timestamp()}}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call({emigrate,_Agent},{Pid,_},cleaning) ->
    exit(Pid,finished),
    {noreply,cleaning,config:arenaTimeout()};

handle_call({emigrate,Agent}, From, State) ->
    {HisPid, _} = From,
    topology_conc:emigrant({Agent,From}),
    %%     {NewCounter,NewLog} = misc_util:arenaReport(State#state.mySupervisor,migration,State#state.lastLog,State#state.counter + 1),
    %%     {noreply,State#state{counter = NewCounter, lastLog = NewLog}}. todo Trzeba odkomentowac dla wysokiej migrationRate
    Emigrants = State#state.emigrants,
    {noreply,State#state{emigrants = [HisPid|Emigrants]},config:arenaTimeout()}.


-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({immigrant,{Agent,From}}, State) ->
    gen_server:reply(From,State#state.arenas),
    {HisPid, _} = From,
    {noreply,State#state{immigrants = [{HisPid,Agent}|State#state.immigrants]},config:arenaTimeout()};

handle_cast(close, _State) ->
    {noreply,cleaning,config:arenaTimeout()}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,cleaning) ->
    {stop,normal,cleaning};

handle_info(timer,cleaning) ->
    {noreply,cleaning,config:writeInterval()/2};

handle_info(timer,State) ->
    conc_supervisor:reportFromArena(State#state.mySupervisor,migration,{State#state.emigrants,State#state.immigrants}), % Dla wysokiej migrationRate trzeba sprawdzac kiedy byl ostatni log
    {noreply,State#state{emigrants = [], immigrants = []},config:arenaTimeout()}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.