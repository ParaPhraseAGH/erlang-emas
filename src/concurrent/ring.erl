%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny walk (ringu).
-module(ring).
-behaviour(gen_server).

%% API
-export([start_link/0, start/0, call/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link() -> {ok,pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec start() -> {ok,pid()}.
start() ->
  gen_server:start(?MODULE, [], []).

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
init([]) ->
  misc_util:seedRandom(),
  {ok, [], config:arenaTimeout()}.

handle_call(_Agent,_From,cleaning) ->
  {reply,0,cleaning,config:arenaTimeout()};
handle_call({_,Fitness,Energy}, From, Waitlist) ->
  Agent = {From,Fitness,Energy},
  case length(Waitlist) == config:fightNumber() - 1 of
    false ->
      {noreply,[Agent|Waitlist],config:arenaTimeout()};
    true ->
      NewAgents = evolution:eachFightsAll([Agent|Waitlist]),
      [gen_server:reply(NewFrom,NewEnergy) || {NewFrom,_,NewEnergy} <- NewAgents],
      {noreply,[],config:arenaTimeout()}
  end.

handle_cast(close, Waitlist) ->
  [gen_server:reply(From,0) || {From,_,_} <- Waitlist],
  {noreply,cleaning,config:arenaTimeout()}.

handle_info(timeout,cleaning) ->
  {stop,normal,cleaning};
handle_info(timeout,Waitlist) ->
  case Waitlist of
    [] ->
      {stop,timeout,Waitlist};
    _ ->
      io:format("Ring ~p daje do walki niepelna liczbe osobnikow!~n",[self()]),
      Agents = evolution:eachFightsAll(Waitlist),
      [gen_server:reply(From,Energy) || {From,_,Energy} <- Agents],
      {noreply,[],config:arenaTimeout()}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
