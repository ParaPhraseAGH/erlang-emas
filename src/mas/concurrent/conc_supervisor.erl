%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul supervisora wyspy w modelu wspolbieznym.

-module(conc_supervisor).
-behaviour(gen_server).

%% API
-export([start/0, go/1, close/1]).
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

-spec go(pid()) -> ok.
go(Pid) ->
    gen_server:cast(Pid, go).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:call(Pid,close,infinity).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {arenas  = dict:new() :: dict:dict(),
                diversity :: pid()}).
-type state() :: #state{}.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([]) ->
    misc_util:seedRandom(),
    Environment = config:agent_env(),
    Interactions = Environment:behaviours(),
    ArenaList = [{Interaction,arena:start_link(self(),Interaction)} || Interaction <- Interactions],
    Arenas = dict:from_list(ArenaList),
    [ok = arena:giveArenas(Pid,Arenas) || {_Interaction,Pid} <- ArenaList],
    io_util:printArenas(ArenaList),
    {ok,#state{arenas = Arenas}}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(close,_From,State) ->
    [arena:close(Pid) || {_Name,Pid} <- dict:to_list(State#state.arenas)],
    {stop,normal,ok,State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.

handle_cast(go, State) ->
    Environment = config:agent_env(),
    Agents = Environment:initial_population(),
    _InitPopulation = [{spawn(agent,start,[A,State#state.arenas]),A} || A <- Agents],
%%     diversity:initPopulation(State#state.diversity,InitPopulation),
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