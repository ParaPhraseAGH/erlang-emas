%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Serwer topology odpowiada za przechowywanie informacji o topologii wysp w systemie.
%% Wyznacza rowniez miejsca docelowe migracji dla procesow.
-module(topology).
-behaviour(gen_server).

%% API
-export([start_link/2, getDestination/1, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-type topology() :: mesh | ring.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(integer(),topology()) -> {ok,pid()}.
start_link(IslandsNr,Topology) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [IslandsNr,Topology], []).

-spec close() -> ok.
close() ->
  gen_server:cast(whereis(?MODULE),close).

-spec getDestination(integer()) -> integer().
%% @doc Funkcja komunikujaca sie synchronicznie z procesem topology i zwracajaca docelowa wyspe
%%  na podstawie przeslanego argumentu
getDestination(X) ->
  gen_server:call(whereis(?MODULE),{destination,X}).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {n,topology}).

init([IslandsNr,Topology]) ->
  misc_util:seedRandom(),
  {ok, #state{n = IslandsNr, topology = Topology}}.

handle_call({destination, X}, _From, State) ->
    Ans = case State#state.n of
              1 -> 1;
              N ->
                  case State#state.topology of
                      ring ->
                          NewIsland = case random:uniform() < 0.5 of
                                          true -> X + 1;
                                          false -> X - 1
                                      end,
                          case NewIsland - 1 of
                              -1 -> N;
                              N -> 1;
                              _ -> NewIsland
                          end;
                      mesh ->
                          Destinations = [I || I <- lists:seq(1,N), I =/= X],
                          Index = random:uniform(length(Destinations)),
                          lists:nth(Index,Destinations);
                      _ ->
                          erlang:error(wrongTopology)
                  end
          end,
    {reply, Ans, State}.

handle_cast(close, State) ->
  {stop, normal, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
