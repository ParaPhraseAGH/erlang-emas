%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
-module(port).
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, call/1, close/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Supervisor,King) ->
  gen_server:start_link(?MODULE, [Supervisor,King], []).

start(Supervisor,King) ->
  gen_server:start(?MODULE, [Supervisor,King], []).

call(Pid) ->
  gen_server:call(Pid,emigrate).

close(Pid) ->
  gen_server:cast(Pid,close).

%% gen_server callbacks
-record(state, {mySupervisor,allSupervisors}).

init(Args) ->
  random:seed(erlang:now()),
  self() ! Args, %trik, zeby nie bylo deadlocka. Musimy zakonczyc funkcje init, zeby odblokowac supervisora i
  {ok, #state{mySupervisor = undefined, allSupervisors = undefined},config:arenaTimeout()}.

handle_call(emigrate,{Pid,_},cleaning) ->
  exit(Pid,finished),
  {noreply,cleaning,config:arenaTimeout()};
handle_call(emigrate, From, State) ->
  {HisPid, _} = From,
  IslandFrom = misc_util:index(State#state.mySupervisor,State#state.allSupervisors),
  IslandTo = topology:getDestination(IslandFrom),
  NewSupervisor = lists:nth(IslandTo,State#state.allSupervisors),
  case {conc_supervisor:unlinkAgent(State#state.mySupervisor,HisPid),conc_supervisor:linkAgent(NewSupervisor,From)} of
    {ok,ok} -> migrationSuccessful;
    _ -> exit(HisPid,finished)
  end,
  {noreply,State,config:arenaTimeout()}.

handle_cast(close, _State) ->
  {noreply,cleaning,config:arenaTimeout()}.

handle_info(timeout,cleaning) ->
  {stop,normal,cleaning};
handle_info(timeout,State) ->
  {stop,timeout,State};
handle_info([Supervisor,King], _UndefinedState) ->
  AllSupervisors = concurrent:getAddresses(King),
  {noreply, #state{mySupervisor = Supervisor, allSupervisors = AllSupervisors},config:arenaTimeout()}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
