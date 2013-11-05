%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny migracji (portu).
-module(port).
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, call/1, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid(),pid()) -> {ok,pid()}.
start_link(Supervisor,King) ->
    gen_server:start_link(?MODULE, [Supervisor,King], []).

-spec start(pid(),pid()) -> {ok,pid()}.
start(Supervisor,King) ->
    gen_server:start(?MODULE, [Supervisor,King], []).

-spec call(pid()) -> [pid()].
%% @doc Funkcja wysylajaca zgloszenie agenta do portu.
call(Pid) ->
    gen_server:call(Pid,emigrate).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {mySupervisor :: pid(),
                allSupervisors :: [pid()],
                counter = 0 :: non_neg_integer()}).
-type state() :: #state{} | cleaning.

-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init(Args) ->
    misc_util:seedRandom(),
    self() ! Args, %trik, zeby nie bylo deadlocka. Musimy zakonczyc funkcje init, zeby odblokowac supervisora i kinga
    timer:send_after(config:writeInterval(),report),
    {ok, #state{mySupervisor = undefined, allSupervisors = undefined}}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(emigrate,{Pid,_},cleaning) ->
    exit(Pid,finished),
    {noreply,cleaning,config:arenaTimeout()};
handle_call(emigrate, From, State) ->
    {HisPid, _} = From,
    IslandFrom = misc_util:find(State#state.mySupervisor,State#state.allSupervisors),
    case catch topology:getDestination(IslandFrom) of
        IslandTo when is_integer(IslandTo) ->
            NewSupervisor = lists:nth(IslandTo,State#state.allSupervisors),
            case catch {conc_supervisor:unlinkAgent(State#state.mySupervisor,HisPid),conc_supervisor:linkAgent(NewSupervisor,From)} of
                {ok,ok} -> migrationSuccessful;
                _ -> exit(HisPid,finished)
            end;
        _ -> exit(HisPid,finished)
    end,
    OldCounter = State#state.counter,
    {noreply,State#state{counter = OldCounter + 1}}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast(close, _State) ->
    {noreply,cleaning,config:arenaTimeout()}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,cleaning) ->
    {stop,normal,cleaning};
handle_info(report,cleaning) ->
    {noreply,cleaning,config:arenaTimeout()};
handle_info(report,State) ->
    conc_supervisor:reportFromArena(State#state.mySupervisor,migration,State#state.counter),
    timer:send_after(config:writeInterval(),report),
    {noreply,State#state{counter = 0}};
handle_info([Supervisor,King], _UndefinedState) ->
    AllSupervisors = concurrent:getAddresses(King),
    {noreply, #state{mySupervisor = Supervisor, allSupervisors = AllSupervisors}}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.