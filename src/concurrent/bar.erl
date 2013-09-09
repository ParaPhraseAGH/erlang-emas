%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
-module(bar).
-behaviour(gen_server).

%% API
-export([start_link/1, start/1, call/2, close/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Supervisor) ->
  gen_server:start_link(?MODULE, [Supervisor], []).

start(Supervisor) ->
  gen_server:start(?MODULE, [Supervisor], []).

call(Pid,Agent) ->
  gen_server:call(Pid,Agent).

close(Pid) ->
  gen_server:cast(Pid,close).

%% gen_server callbacks
-record(state, {supervisor,waitlist=[]}).

init([Supervisor]) ->
  random:seed(erlang:now()),
  {ok, #state{supervisor = Supervisor, waitlist = []},config:arenaTimeout()}.

handle_call(_Agent,_From,cleaning) ->
  {reply,0,cleaning,config:arenaTimeout()};
handle_call(Agent1, From1, State) ->
  case State#state.waitlist of
    [] ->
      {noreply,State#state{waitlist = [{From1,Agent1}]},config:arenaTimeout()};
    [{From2,Agent2}] ->
      [{_,_,NewEnergy1},{_,_,NewEnergy2},NewAgent1,NewAgent2] = evolution:doReproduce({Agent1,Agent2}),
      gen_server:reply(From1,NewEnergy1),
      gen_server:reply(From2,NewEnergy2),
      conc_supervisor:sendAgents(State#state.supervisor,[NewAgent1,NewAgent2]),
      {noreply,State#state{waitlist = []},config:arenaTimeout()}
  end.

handle_cast(close, State) ->
  [gen_server:reply(From,0) || {From,_} <- State#state.waitlist],
  {noreply,cleaning,config:arenaTimeout()}.

handle_info(timeout,cleaning) ->
  {stop,normal,cleaning};
handle_info(timeout,State) ->
  case State#state.waitlist of
    [] ->
      {stop,timeout,State};
    [{From,Agent}] ->
      io:format("Bar ~p reprodukuje pojedynczego osobnika!~n",[self()]),
      [{_,_,NewEnergy},NewAgent] = evolution:doReproduce({Agent}),
      gen_server:reply(From,NewEnergy),
      conc_supervisor:sendAgents(State#state.supervisor,[NewAgent]),
      {noreply,State#state{waitlist = []},config:arenaTimeout()}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
