%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny reprodukcji (baru).
-module(bar).
-behaviour(gen_server).

%% API
-export([start_link/1, start/1, call/2, getStats/1, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid()) -> {ok,pid()}.
start_link(Supervisor) ->
  gen_server:start_link(?MODULE, [Supervisor], []).

-spec start(pid()) -> {ok,pid()}.
start(Supervisor) ->
  gen_server:start(?MODULE, [Supervisor], []).

-spec call(pid(),agent()) -> Energy :: integer().
%% @doc Funkcja wysylajaca zgloszenie agenta do baru
call(Pid,Agent) ->
  gen_server:call(Pid,Agent).

-spec getStats(pid()) -> non_neg_integer().
%% @doc Zwraca statystyki areny
getStats(Pid) ->
  gen_server:call(Pid,getStats).

-spec close(pid()) -> ok.
close(Pid) ->
  gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {supervisor :: pid(),
                waitlist = [] :: [tuple()],
                counter = 0 :: non_neg_integer()}).

init([Supervisor]) ->
  misc_util:seedRandom(),
  {ok, #state{supervisor = Supervisor, waitlist = []},config:arenaTimeout()}.

handle_call(getStats,_From,State) ->
  {reply,State#state.counter,State#state{counter = 0}};
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
      OldCounter = State#state.counter,
      {noreply,State#state{waitlist = [], counter = OldCounter + 1},config:arenaTimeout()}
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
