%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc This is the main module of hybrid model. It handles starting the system, migrating agents and cleaning after work

-module(hybrid).
-behaviour(gen_server).

%% API
-export([start/3,  sendAgent/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include ("mas.hrl").


-type state() :: [pid()].

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(Time::pos_integer(), sim_params(), config()) -> ok.
start(Time, SP, Cf) ->
%%     io:format("{Model=Hybrid,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    {ok, _} = gen_server:start({local,?MODULE}, ?MODULE, [Time, SP, Cf], []),
    timer:sleep(Time).


%% @doc Asynchronously sends an agent from an arena to the supervisor
-spec sendAgent(agent()) -> ok.
sendAgent(Agent) ->
    gen_server:cast(whereis(?MODULE), {agent,self(),Agent}).

%% ====================================================================
%% Callbacks
%% ====================================================================
-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([Time, SP, Cf = #config{islands = Islands}]) ->
    timer:send_after(Time,theEnd),
    Pids = [spawn_link(hybrid_island, start, [SP, Cf]) || _ <- lists:seq(1, Islands)],
    topology:start_link(self(), Islands, Cf#config.topology),
    logger:start_link(Pids, Cf),
    {ok,Pids}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(_,_,State) ->
    {noreply,State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({agent,From,Agent},Pids) ->
    IslandFrom = misc_util:find(From,Pids),
    IslandTo = topology:getDestination(IslandFrom),
    hybrid_island:sendAgent(lists:nth(IslandTo,Pids),Agent),
    {noreply,Pids}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(theEnd,Pids) ->
    {stop,normal,Pids}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason,Pids) ->
    [hybrid_island:close(Pid) || Pid <- Pids],
    topology:close(),
    logger:close().

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn,State,_Extra) ->
    {ok, State}.
