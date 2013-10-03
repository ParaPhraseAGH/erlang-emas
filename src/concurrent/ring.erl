%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny walk (ringu).
-module(ring).
-behaviour(gen_server).

%% API
-export([start_link/1, start/1, call/2, close/1]).
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
%% @doc Funkcja wysylajaca zgłoszenie agenta do ringu.
call(Pid,Agent) ->
  gen_server:call(Pid,Agent).

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
  timer:send_after(config:writeInterval(),report),
  {ok, #state{supervisor = Supervisor}, config:arenaTimeout()}.

handle_call(_Agent,_From,cleaning) ->
  {reply,0,cleaning,config:arenaTimeout()};
handle_call({_,Fitness,Energy}, From, State) ->
  Agent = {From,Fitness,Energy},
  Waitlist = State#state.waitlist,
  case length(Waitlist) == config:fightNumber() - 1 of
    false ->
      {noreply,State#state{waitlist = [Agent|Waitlist]},config:arenaTimeout()};
    true ->
      NewAgents = evolution:eachFightsAll([Agent|Waitlist]),
      [gen_server:reply(NewFrom,NewEnergy) || {NewFrom,_,NewEnergy} <- NewAgents],
      NewCounter = State#state.counter + (config:fightNumber() * (config:fightNumber() - 1) div 2), % liczba spotkan dla wektora dlugosci n wynosi n(n-1)/2
      {noreply,State#state{waitlist = [], counter = NewCounter},config:arenaTimeout()}
  end.

handle_cast(close, State) ->
  [gen_server:reply(From,0) || {From,_,_} <- State#state.waitlist],
  {noreply,cleaning,config:arenaTimeout()}.

handle_info(report, State) ->
  conc_supervisor:report(State#state.supervisor,State#state.counter,fight),
  timer:send_after(config:writeInterval(),report),
  {noreply,State#state{counter = 0},config:arenaTimeout()};
handle_info(timeout,cleaning) ->
  {stop,normal,cleaning};
handle_info(timeout,State) ->
  case State#state.waitlist of
    [] ->
      {stop,timeout,State};
    X ->
      io:format("Ring ~p daje do walki niepelna liczbe osobnikow!~n",[self()]),
      Agents = evolution:eachFightsAll(X),
      [gen_server:reply(From,Energy) || {From,_,Energy} <- Agents],
      {noreply,State#state{waitlist = []},config:arenaTimeout()}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
