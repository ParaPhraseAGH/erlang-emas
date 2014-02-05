%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul supervisora wyspy w modelu wspolbieznym.

-module(conc_supervisor).
-behaviour(gen_server).

%% API
-export([start/0, go/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> pid().
start() ->
    {ok,Pid} = gen_server:start(?MODULE,[],[]),
    Pid.

-spec go(pid(),non_neg_integer()) -> ok.
go(Pid,ProblemSize) ->
    gen_server:cast(Pid,{go,ProblemSize}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:call(Pid,close,infinity).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {arenas :: [pid()],
                diversity :: pid()}).
-type state() :: #state{}.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([]) ->
    misc_util:seedRandom(),
    {ok,Diversity} = diversity:start_link(self()),
    {ok,Cemetery} = cemetery:start_link(self(),Diversity),
    {ok,Ring} = ring:start_link(self()),
    {ok,Bar} = bar:start_link(self(),Diversity),
    {ok,Port} = port:start_link(self(),Diversity),
    Arenas = [Ring,Bar,Port,Cemetery],
    bar:giveArenas(Bar,Arenas),
    port:giveArenas(Port,Arenas),
    io_util:printArenas(Arenas),
    {ok,#state{arenas = Arenas, diversity = Diversity}}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(close,_From,State) ->
    [Ring,Bar,Port,Cemetery] = State#state.arenas,
    port:close(Port),
    bar:close(Bar),
    ring:close(Ring),
    cemetery:close(Cemetery),
    diversity:close(State#state.diversity),
    {stop,normal,ok,State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.

handle_cast({go,ProblemSize},State) ->
    Agents = [genetic:generateAgent(ProblemSize) || _ <- lists:seq(1,config:populationSize())],
    InitPopulation = [{spawn(agent,start,[A,State#state.arenas]),A} || A <- Agents],
    diversity:initPopulation(State#state.diversity,InitPopulation),
    {noreply,State}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,State) ->
    {stop,timeout,State}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason,_State) ->
    ok.


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn,State,_Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================