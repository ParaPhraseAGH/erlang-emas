%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul areny reprodukcji (baru).
-module(bar).
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

%% @doc Funkcja wysylajaca zgloszenie agenta do baru
-spec call(pid(),agent()) -> Energy :: integer().
call(Pid,Agent) ->
    gen_server:call(Pid,Agent).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {supervisor :: pid(),
                waitlist = [] :: [tuple()],
                newborns = [] :: [pid()],
                best = -999999.9 :: float(),
                lastLog :: erlang:timestamp()}).
-type state() :: #state{} | cleaning.

-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([Supervisor]) ->
    misc_util:seedRandom(),
    {ok, #state{supervisor = Supervisor, lastLog = os:timestamp()},config:arenaTimeout()}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(_Agent,_From,cleaning) ->
    {reply,0,cleaning,config:arenaTimeout()};
handle_call(Agent1, From1, State) ->
    case State#state.waitlist of
        [] ->
            {noreply,State#state{waitlist = [{From1,Agent1}]},config:arenaTimeout()};
        [{From2,Agent2}] ->
            [{_,_,NewEnergy1},{_,_,NewEnergy2},NewAgent1,NewAgent2] = evolution:doReproduce({Agent1,Agent2}),
            gen_server:reply(From1,NewEnergy1),
            gen_server:reply(From2,NewEnergy2),
            AgentList = [NewAgent1,NewAgent2],
            NewBest = lists:max([misc_util:result(AgentList),State#state.best]),
            Pids = [spawn(agent,start,[A,State#state.supervisor]) || A <- AgentList],
            NewNewborns = lists:append(State#state.newborns,lists:zip(Pids,AgentList)),
            {NewCounter,NewLog} = misc_util:arenaReport(State#state.supervisor,reproduction,State#state.lastLog,{NewBest,NewNewborns}),
            case NewCounter of
                0 ->
                    {noreply,State#state{waitlist = [],
                                         lastLog = NewLog,
                                         newborns = [],
                                         best = NewBest},config:arenaTimeout()};
                _ ->
                    {noreply,State#state{waitlist = [],
                                         newborns = NewNewborns,
                                         best = NewBest},config:arenaTimeout()}
            end
    end.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast(close, State) ->
    [gen_server:reply(From,0) || {From,_} <- State#state.waitlist],
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
        [{From,Agent}] ->
            io:format("Bar ~p reprodukuje pojedynczego osobnika!~n",[self()]),
            [{_,_,NewEnergy},NewAgent] = evolution:doReproduce({Agent}),
            gen_server:reply(From,NewEnergy),
            NewBest = lists:max([misc_util:result([NewAgent]),State#state.best]),
            Pid = spawn(agent,start,[NewAgent,State#state.supervisor]),
            NewNewborns = [{Pid,NewAgent}|State#state.newborns],
            {NewCounter,NewLog} = misc_util:arenaReport(State#state.supervisor,reproduction,State#state.lastLog,{NewBest,NewNewborns}),
            case NewCounter of
                0 ->
                    {noreply,State#state{waitlist = [],
                                         lastLog = NewLog,
                                         newborns = [],
                                         best = NewBest},config:arenaTimeout()};
                _ ->
                    {noreply,State#state{waitlist = [],
                                         newborns = NewNewborns,
                                         best = NewBest},config:arenaTimeout()}
            end
    end.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
