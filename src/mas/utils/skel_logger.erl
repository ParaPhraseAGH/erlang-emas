-module(skel_logger).
-behaviour(gen_server).

%% API
-export([start_link/1, reportResult/2, close/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {dict  = dict:new() :: dict(),
                lastFitness = 0 :: number(),
                lastPopulation = 0 :: number(),
                fights = 0 :: number(),
                reproductions = 0 :: number(),
                deaths = 0 ::number()}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(string()) -> {ok, pid()}.
start_link(Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).

-spec reportResult(atom(),term()) -> ok.
reportResult(Stat,Value) ->
    gen_server:cast(?MODULE,{Stat,Value}).

-spec close() -> ok.
close() ->
    gen_server:cast(whereis(?MODULE), close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init(term()) -> {ok,state()}.
init([Path]) ->
    timer:send_interval(config:writeInterval(),write),
    Dictionary = lists:foldl(fun(Atom, Dict) ->
                                     Filename = atom_to_list(Atom) ++ ".txt",
                                     {ok, Descriptor} = file:open(filename:join([Path, Filename]), [append, delayed_write, raw]),
                                     dict:store(Atom, Descriptor, Dict)
                             end, dict:new(),
                             [fitness,population,reproduction,fight,death]),

    {ok, #state{dict = Dictionary}}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({fitness,Value}, State) ->
    {noreply, State#state{lastFitness = Value}};
handle_cast({population,Value}, State) ->
    {noreply, State#state{lastPopulation = Value}};
handle_cast({fight,Value}, State) ->
    OldValue = State#state.fights,
    {noreply, State#state{fights = Value + OldValue}};
handle_cast({reproduce,Value}, State) ->
    OldValue = State#state.reproductions,
    {noreply, State#state{reproductions = Value + OldValue}};
handle_cast({death,Value}, State) ->
    OldValue = State#state.deaths,
    {noreply, State#state{deaths = Value + OldValue}};
handle_cast(close, State) ->
    {stop, normal, State}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(write, State) ->
    file:write(standard_io, io_lib:fwrite("~p ~p ~p\n", [fitness, 1, State#state.lastFitness])),
    file:write(standard_io, io_lib:fwrite("~p ~p ~p\n", [population, 1, State#state.lastPopulation])),
    file:write(standard_io, io_lib:fwrite("~p ~p\n", [death, State#state.deaths])),
    file:write(standard_io, io_lib:fwrite("~p ~p\n", [reproduction, State#state.reproductions])),
    file:write(standard_io, io_lib:fwrite("~p ~p\n", [fight, State#state.fights])),
    {noreply, State#state{fights = 0, deaths = 0, reproductions = 0}}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, State) ->
    closeFiles(State#state.dict).

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec print(float(),non_neg_integer()) -> ok.
print(Fitness,Population) ->
%%     io:format("Fitness: ~p Population: ~p~n", [Fitness,Population]).
ok.
-spec write(file:io_device(),term()) -> ok.
write(FD,Value) ->
    file:write(standard_io, io_lib:fwrite("~p\n", [Value])).

-spec closeFiles(dict()) -> list().
closeFiles(Dict) ->
    [file:close(FD) || {_Key,FD} <- dict:to_list(Dict)].
