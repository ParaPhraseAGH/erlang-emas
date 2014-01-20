%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny migracji (portu).
-module(port).
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, call/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid(),pid()) -> {ok,pid()}.
start_link(Supervisor,King) ->
    gen_server:start_link(?MODULE, [Supervisor,King], []).

-spec start(pid(),pid()) -> {ok,pid()}.
start(Supervisor,King) ->
    gen_server:start(?MODULE, [Supervisor,King], []).

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
                allSupervisors :: [pid()],
                counter = 0 :: non_neg_integer(),
                lastLog :: erlang:timestamp()}).
-type state() :: #state{} | cleaning.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init(Args) ->
    misc_util:seedRandom(),
    self() ! Args, %trik, zeby nie bylo deadlocka. Musimy zakonczyc funkcje init, zeby odblokowac supervisora i kinga
    timer:send_interval(config:writeInterval(),timer),
    {ok, #state{mySupervisor = undefined, allSupervisors = undefined, lastLog = os:timestamp()}}.


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
    IslandFrom = misc_util:find(State#state.mySupervisor,State#state.allSupervisors),
    case catch topology:getDestination(IslandFrom) of
        IslandTo when is_integer(IslandTo) ->
            NewSupervisor = lists:nth(IslandTo,State#state.allSupervisors),
            case catch {conc_supervisor:unlinkAgent(State#state.mySupervisor,HisPid,Agent),conc_supervisor:linkAgent(NewSupervisor,From,Agent)} of
                {ok,ok} -> migrationSuccessful;
                _ -> exit(HisPid,finished)
            end;
        _ -> exit(HisPid,finished)
    end,
%%     {NewCounter,NewLog} = misc_util:arenaReport(State#state.mySupervisor,migration,State#state.lastLog,State#state.counter + 1),
%%     {noreply,State#state{counter = NewCounter, lastLog = NewLog}}. todo Trzeba odkomentowac dla wysokiej migrationRate
    Counter = State#state.counter,
    {noreply,State#state{counter = Counter + 1}}.


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
handle_info(timer,cleaning) ->
    {noreply,cleaning,config:writeInterval()/2};
handle_info(timer,State) ->
    conc_supervisor:reportFromArena(State#state.mySupervisor,migration,State#state.counter), % Dla wysokiej migrationRate trzeba sprawdzac kiedy byl ostatni log
    {noreply,State#state{counter = 0},config:arenaTimeout()};
handle_info({Ref,ok},State) when is_reference(Ref) ->
    %%   Opoznione przyjscie potwierdzenia od supervisora. Juz i tak jest po wszystkim.
    {noreply,State};
handle_info([Supervisor,King], State) ->
    AllSupervisors = concurrent:getAddresses(King),
    {noreply, State#state{mySupervisor = Supervisor, allSupervisors = AllSupervisors}}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.