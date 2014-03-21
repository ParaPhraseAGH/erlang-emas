%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny walk (ringu).
-module(ring).
-behaviour(gen_server).

%% API
-export([start_link/1, start/1, call/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid()) -> {ok,pid()}.
start_link(Supervisor) ->
    gen_server:start_link(?MODULE, [Supervisor], []).

-spec start(pid()) -> {ok,pid()}.
start(Supervisor) ->
    gen_server:start(?MODULE, [Supervisor], []).

%% @doc Funkcja wysylajaca zgloszenie agenta do ringu.
-spec call(pid(),agent()) -> Energy :: integer().
call(Pid,Agent) ->
    gen_server:call(Pid,Agent,infinity).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {supervisor :: pid(),
                waitlist = [] :: [tuple()],
                counter = 0 :: non_neg_integer(),
                lastLog :: erlang:timestamp()}).
-type state() :: #state{} | cleaning.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([Supervisor]) ->
    misc_util:seedRandom(),
    {ok, #state{supervisor = Supervisor, lastLog = os:timestamp()}, config:arenaTimeout()}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(_Agent,_From,cleaning) ->
    {reply,0,cleaning,config:arenaTimeout()};
handle_call({_,Fitness,Energy}, From, State) ->
    Agent = {From,Fitness,Energy},
    Waitlist = State#state.waitlist,
    case length(Waitlist) == emas_config:fightNumber() - 1 of
        false ->
            {noreply,State#state{waitlist = [Agent|Waitlist]},config:arenaTimeout()};
        true ->
            NewAgents = evolution:eachFightsAll([Agent|Waitlist]),
            [gen_server:reply(NewFrom,NewEnergy) || {NewFrom,_,NewEnergy} <- NewAgents],
            Counter = State#state.counter + (emas_config:fightNumber() * (emas_config:fightNumber() - 1) div 2), % liczba spotkan dla wektora dlugosci n wynosi n(n-1)/2
            case misc_util:logNow(State#state.lastLog) of
                {yes,NewLog} ->
                    conc_logger:log(State#state.supervisor,fight,Counter),
                    {noreply,State#state{lastLog = NewLog,
                                         waitlist = [],
                                         counter = 0},config:arenaTimeout()};
                notyet ->
                    {noreply,State#state{waitlist = [],
                                         counter = Counter},config:arenaTimeout()}
            end
    end.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast(close, State) ->
    [gen_server:reply(From,0) || {From,_,_} <- State#state.waitlist],
    {noreply,cleaning,config:arenaTimeout()}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,cleaning) ->
    {stop,normal,cleaning};
handle_info(timeout,State) ->
    case State#state.waitlist of
        [] ->
            {stop,timeout,State};
        X ->
            io:format("Ring ~p daje do walki niepelna liczbe osobnikow!~n",[self()]),
            Agents = evolution:eachFightsAll(X),
            [gen_server:reply(From,Energy) || {From,_,Energy} <- Agents],
            Counter = State#state.counter + (length(X) * (length(X) - 1) div 2), % liczba spotkan dla wektora dlugosci n wynosi n(n-1)/2
            case misc_util:logNow(State#state.lastLog) of
                {yes,NewLog} ->
                    conc_logger:log(State#state.supervisor,fight,Counter),
                    {noreply,State#state{lastLog = NewLog,
                                         waitlist = [],
                                         counter = 0},config:arenaTimeout()};
                notyet ->
                    {noreply,State#state{waitlist = [],
                                         counter = Counter},config:arenaTimeout()}
            end
    end.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
