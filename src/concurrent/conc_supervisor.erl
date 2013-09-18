%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul supervisora wyspy w modelu wspolbieznym.

-module(conc_supervisor).
-behaviour(gen_server).

%% API
-export([start/4, sendAgents/2, unlinkAgent/2, linkAgent/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(King::pid(), N::pos_integer(), Path::string(), ProblemSize::pos_integer()) -> pid().
start(King,N,Path,ProblemSize) ->
  {ok,Pid} = gen_server:start(?MODULE,[King,N,Path,ProblemSize],[]),
  Pid.

-spec sendAgents(pid(),[agent()]) -> ok.
%% @doc Funkcja za pomocą której można wysłać supervisorowi listę nowych agentów.
sendAgents(Pid,Agents) ->
  gen_server:cast(Pid,{newAgents,Agents}).

-spec unlinkAgent(pid(),pid()) -> ok.
%% @doc Funkcja usuwa link między supervisorem, a danym agentem. Zapytanie synchroniczne.
unlinkAgent(Pid,AgentPid) ->
  gen_server:call(Pid,{emigrant,AgentPid}).

-spec linkAgent(pid(),{pid(),reference()}) -> ok.
%% @doc Funkcja tworzy link między supervisorem, a danym agentem. Zapytanie synchroniczne.
linkAgent(Pid,AgentFrom) ->
  gen_server:call(Pid,{immigrant,AgentFrom}).

-spec close(pid()) -> ok.
close(Pid) ->
  gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {best = -999999.9 :: float(),
  fds :: dict(),
  population = config:populationSize() :: pos_integer(),
  arenas :: [pid()]}).

init([King,N,Path,ProblemSize]) ->
  misc_util:seedRandom(),
  process_flag(trap_exit, true),
  {ok,Ring} = ring:start(),
  {ok,Bar} = bar:start(self()),
  {ok,Port} = port:start(self(),King),
  Arenas = [Ring,Bar,Port],
  io_util:printArenas(Arenas),
  [spawn_link(agent,start,[ProblemSize|Arenas]) || _ <- lists:seq(1,config:populationSize())],
  IslandPath = filename:join([Path,"isl" ++ integer_to_list(N)]),
  FDs = io_util:prepareWriting(IslandPath),
  timer:send_after(config:writeInterval(),{write,-99999}),
  {ok,#state{fds = FDs, arenas = Arenas},config:supervisorTimeout()}.

terminate(_Reason,State) ->
  [Ring,Bar,Port] = State#state.arenas,
  port:close(Port),
  bar:close(Bar),
  ring:close(Ring),
  io_util:closeFiles(State#state.fds).

handle_call({emigrant,AgentPid},_From,State) ->
  erlang:unlink(AgentPid),
  Population = State#state.population,
  {reply,ok,State#state{population = Population - 1}};
handle_call({immigrant,AgentFrom},_From,State) ->
  {AgentPid,_} = AgentFrom,
  erlang:link(AgentPid),
  gen_server:reply(AgentFrom,State#state.arenas),
  Population = State#state.population,
  {reply,ok,State#state{population = Population + 1}}.


handle_cast({newAgents,AgentList},State) ->
  [spawn_link(agent,start,[A|State#state.arenas]) || A <- AgentList],
  Result = misc_util:result(AgentList),
  NewPopulation = State#state.population + length(AgentList),
  Best = State#state.best,
  {noreply,State#state{best = lists:max([Result,Best]), population = NewPopulation},config:supervisorTimeout()};
handle_cast(close,State) ->
  {stop,normal,State}.


handle_info({'EXIT',_,_},State) ->
  Population = State#state.population,
  {noreply,State#state{population = Population - 1},config:supervisorTimeout()};
handle_info({write,Last},State) ->
  Fitness = case State#state.best of
   islandEmpty -> Last;
   X -> X
  end,
  io_util:write(dict:fetch(fitness,State#state.fds),Fitness),
  io_util:write(dict:fetch(population,State#state.fds),State#state.population),
  io:format("Island ~p Fitness ~p Population ~p~n",[self(),State#state.best,State#state.population]),
  timer:send_after(config:writeInterval(),{write,Fitness}),
  {noreply,State,config:supervisorTimeout()};
handle_info(timeout,State) ->
  {stop,timeout,State}.

code_change(_OldVsn,State,_Extra) ->
  {ok, State}.
