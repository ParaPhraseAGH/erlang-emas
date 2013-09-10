%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy.

-module(conc_supervisor).
-behaviour(gen_server).
-export([start/4, sendAgents/2, unlinkAgent/2, linkAgent/2, close/1,
  init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(King::pid(), N::pos_integer(), Path::string(), ProblemSize::pos_integer()) -> pid().
start(King,N,Path,ProblemSize) ->
  {ok,Pid} = gen_server:start(?MODULE,[King,N,Path,ProblemSize],[]),
  Pid.

init([King,N,Path,ProblemSize]) ->
  random:seed(erlang:now()),
  process_flag(trap_exit, true),
  {ok,Port} = port:start(self(),King),
  {ok,Ring} = ring:start(),
  {ok,Bar} = bar:start(self()),
  Arenas = [Ring,Bar,Port],
  [spawn_link(agent,start,[ProblemSize|Arenas]) || _ <- lists:seq(1,config:populationSize())],
  IslandPath = filename:join([Path,"isl" ++ integer_to_list(N)]),
  FDs = io_util:prepareWriting(IslandPath),
  timer:send_after(config:writeInterval(),write),
  {ok,{-999999999,FDs,config:populationSize(),Arenas},config:supervisorTimeout()}.

terminate(_Reason,{_Best,FDs,_,[Ring,Bar,Port]}) ->
  port:close(Port),
  bar:close(Bar),
  ring:close(Ring),
  io_util:closeFiles(FDs).

-spec sendAgents(pid(),[agent()]) -> ok.
sendAgents(Pid,Agents) ->
  gen_server:cast(Pid,{newAgents,Agents}).

-spec unlinkAgent(pid(),pid()) -> ok.
unlinkAgent(Pid,AgentPid) ->
  gen_server:call(Pid,{emigrant,AgentPid}).

-spec linkAgent(pid(),{pid(),reference()}) -> ok.
linkAgent(Pid,AgentFrom) ->
  gen_server:call(Pid,{immigrant,AgentFrom}).

-spec close(pid()) -> ok.
close(Pid) ->
  gen_server:cast(Pid,close).

handle_call({emigrant,AgentPid},_From,{Best,FDs,Population,Arenas}) ->
  erlang:unlink(AgentPid),
  {reply,ok,{Best,FDs,Population - 1,Arenas}};
handle_call({immigrant,AgentFrom},_From,{Best,FDs,Population,Arenas}) ->
  {AgentPid,_} = AgentFrom,
  erlang:link(AgentPid),
  %AgentPid ! {AgentRef,Arenas},
  gen_server:reply(AgentFrom,Arenas),
  {reply,ok,{Best,FDs,Population + 1,Arenas}}.


handle_cast({newAgents,AgentList},{Best,FDs,Population,Arenas}) ->
  [spawn_link(agent,start,[A|Arenas]) || A <- AgentList],
  Result = misc_util:result(AgentList),
  NewPopulation = Population + length(AgentList),
  {noreply,{lists:max([Result,Best]),FDs,NewPopulation,Arenas},config:supervisorTimeout()};
handle_cast(close,State) ->
  {stop,normal,State}.


handle_info({'EXIT',_,_},{Best,FDs,Population,Arenas}) ->
  {noreply,{Best,FDs,Population - 1,Arenas},config:supervisorTimeout()};
handle_info(write,{Best,FDs,Population,Arenas}) ->
  io_util:write(dict:fetch(fitness,FDs),Best),
  io_util:write(dict:fetch(population,FDs),Population),
  io:format("Island ~p Fitness ~p Population ~p~n",[self(),Best,Population]),
  timer:send_after(config:writeInterval(),write),
  {noreply,{Best,FDs,Population,Arenas},config:supervisorTimeout()};
handle_info(timeout,State) ->
  {stop,timeout,State}.

code_change(_OldVsn,State,_Extra) ->
  {ok, State}.
