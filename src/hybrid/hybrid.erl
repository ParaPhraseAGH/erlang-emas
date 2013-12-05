%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul modelu hybrydowego. Uruchamia algorytm, zarzadza migracja i sprzata po wszystkim.
-module(hybrid).
-behaviour(gen_server).

%% API
-export([start/5, start/1, start/0,  sendAgent/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type state() :: [pid()].

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
    start(40,5000,2,mesh,"tmp").

%% @doc Funkcja za pomoca ktorej wyspa moze wyslac agenta supervisorowi.
%% Komunikacja asynchroniczna - agent jest wysylany i proces idzie dalej nie czekajac na odpowiedz.
-spec sendAgent(agent()) -> ok.
sendAgent(Agent) ->
    gen_server:cast(whereis(?MODULE), {agent,self(),Agent}).

%% ====================================================================
%% Callbacks
%% ====================================================================
-spec init(term()) -> {ok,state()}.
init([ProblemSize,Time,Islands,Topology,Path]) ->
    timer:send_after(Time,theEnd),
    Pids = [spawn_link(hybrid_island,start,[ProblemSize]) || _ <- lists:seq(1,Islands)],
    topology:start_link(Islands,Topology),
    logger:start_link({parallel,Pids},Path),
    {ok,Pids,config:supervisorTimeout()}.

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
    {noreply,Pids,config:supervisorTimeout()}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,Pids) ->
    {stop,timeout,Pids};
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
