%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(hybrid).
-behaviour(gen_server).
-export([sendAgent/1, start/5, start/1, start/0,
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path) ->
  {ok, _} = gen_server:start({local,?MODULE}, ?MODULE, [ProblemSize,Time,Islands,Topology,Path], []),
  timer:sleep(Time).

-spec start(list()) -> ok.
start([A,B,C,D,E]) ->
  start(list_to_integer(A),
    list_to_integer(B),
    list_to_integer(C),
    list_to_atom(D),
    E).

-spec start() -> ok.
start() ->
  file:make_dir("tmp"),
  start(40,5000,3,mesh,"tmp").

-spec sendAgent(agent()) -> ok.
sendAgent(Agent) ->
  gen_server:cast(whereis(?MODULE), {agent,self(),Agent}).

%% @spec init() -> ok
%% @doc Funkcja wykonujaca wszelkie operacje potrzebne przed uruchomieniem
%% algorytmu.
init([ProblemSize,Time,Islands,Topology,Path]) ->
  timer:send_after(Time,theEnd),
  topology:start_link(Islands,Topology),
  Pids = [spawn_link(hybrid_island,start,[Path,X,ProblemSize]) || X <- lists:seq(1,Islands)],
  {ok,Pids,config:supervisorTimeout()}.

handle_call(_,_,State) ->
  {noreply,State}.

handle_cast({agent,From,Agent},Pids) ->
  IslandFrom = misc_util:index(From,Pids),
  IslandTo = topology:getDestination(IslandFrom),
  hybrid_island:sendAgent(lists:nth(IslandTo, Pids),Agent),
  {noreply,Pids,config:supervisorTimeout()}.

handle_info(timeout,Pids) ->
  {stop,timeout,Pids};
handle_info(theEnd,Pids) ->
  {stop,normal,Pids}.

terminate(_Reason,Pids) ->
  [hybrid_island:close(Pid) || Pid <- Pids],
  topology:close().

code_change(_OldVsn,State,_Extra) ->
  {ok, State}.

