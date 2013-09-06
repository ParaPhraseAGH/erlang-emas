%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
-module(topology).

-behaviour(gen_server).

%% API
-export([start_link/2, getDestination/1, close/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(IslandsNr,Topology) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [IslandsNr,Topology], []).

close() ->
  gen_server:cast(whereis(?MODULE),close).

getDestination(X) ->
  gen_server:call(whereis(?MODULE),{destination,X}).

%% gen_server callbacks
-record(state, {n,topology}).

init([IslandsNr,Topology]) ->
  random:seed(erlang:now()),
  {ok, #state{n = IslandsNr, topology = Topology}}.

handle_call({destination,X}, _From, State) ->
  N = State#state.n,
  Ans = case State#state.topology of
    ring ->
      NewIsland = case random:uniform() < 0.5 of
        true -> X + 1;
        false -> X - 1
      end,
      if NewIsland == 0 ->
        N;
      NewIsland == N + 1 ->
        1
      end;
    mesh ->
      random:uniform(N)
  end,
  {reply, Ans ,State}.

handle_cast(close, State) ->
  {stop, normal, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
