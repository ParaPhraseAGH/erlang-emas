%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul supervisora wyspy w modelu wspolbieznym.

-module(conc_supervisor).
-behaviour(gen_server).

%% API
-export([start/2, go/2, newAgent/2, sendAgents/2, unlinkAgent/2, linkAgent/2, reportFromArena/3, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(King::pid()) -> pid().
start(King) ->
    {ok,Pid} = gen_server:start(?MODULE,[King],[]),
    Pid.

-spec newAgent(pid(),agent()) -> ok.
newAgent(Pid,Agent) ->
    gen_server:cast(Pid,{newAgent,self(),Agent}).

-spec go(pid(),non_neg_integer()) -> ok.
go(Pid,ProblemSize) ->
    gen_server:cast(Pid,{go,ProblemSize}).

%% @doc Funkcja za pomoca ktorej mozna wyslac supervisorowi liste nowych agentow.
-spec sendAgents(pid(),[agent()]) -> ok.
sendAgents(Pid,Agents) ->
    gen_server:cast(Pid,{newAgents,Agents}).

%% @doc Funkcja usuwa link miedzy supervisorem, a danym agentem. Zapytanie synchroniczne.
-spec unlinkAgent(pid(),pid(),agent()) -> ok.
unlinkAgent(Pid,AgentPid,Agent) ->
    gen_server:call(Pid,{emigrant,AgentPid,Agent}).

%% @doc Funkcja tworzy link miedzy supervisorem, a danym agentem. Zapytanie synchroniczne.
-spec linkAgent(pid(),{pid(),reference()},agent()) -> ok.
linkAgent(Pid,AgentFrom,Agent) ->
    gen_server:call(Pid,{immigrant,AgentFrom,Agent}).

-spec reportFromArena(pid(),fight | reproduction | migration, non_neg_integer()) -> ok.
reportFromArena(Pid,Arena,Value) ->
    gen_server:cast(Pid,{reportFromArena,Arena,Value}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {best = -999999.9 :: float() | islandEmpty,
                agents = gb_trees:empty() :: gb_tree(),
                deathCounter = 0 :: non_neg_integer(),
                reports = dict:new() :: dict(),
                arenas :: [pid()]}).
-type state() :: #state{}.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([King]) ->
    misc_util:seedRandom(),
    process_flag(trap_exit, true),
    {ok,Ring} = ring:start_link(self()),
    {ok,Bar} = bar:start_link(self()),
    {ok,Port} = port:start_link(self(),King),
    Arenas = [Ring,Bar,Port],
    io_util:printArenas(Arenas),
    {ok,#state{arenas = Arenas},config:supervisorTimeout()}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call({emigrant,AgentPid,_Agent},_From,State) ->
    erlang:unlink(AgentPid),
    NewAgentList = gb_trees:delete(AgentPid,State#state.agents), % delete_any/2 is safe, this one crashes when not found
    {reply,ok,State#state{agents = NewAgentList}};

handle_call({immigrant,AgentFrom,Agent},_From,State) ->
    {AgentPid,_} = AgentFrom,
    erlang:link(AgentPid),
    gen_server:reply(AgentFrom,State#state.arenas),
    NewAgentList = gb_trees:insert(AgentPid,Agent,State#state.agents),
    {reply,ok,State#state{agents = NewAgentList}}.


-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({newAgents,AgentList},State) ->
    Pids = [spawn_link(agent,start,[A|State#state.arenas]) || A <- AgentList],
    Result = misc_util:result(AgentList),
    NewAgentList = lists:foldl(fun({Pid,Agent},Tree) ->
                                       gb_trees:insert(Pid,Agent,Tree)
                               end,State#state.agents,lists:zip(Pids,AgentList)),
    {noreply,State#state{best = lists:max([Result,State#state.best]),
                         agents = NewAgentList},config:supervisorTimeout()};

handle_cast({newAgent,Pid,Agent},State) ->
    NewAgentList = gb_trees:insert(Pid,Agent,State#state.agents),
    {noreply,State#state{agents = NewAgentList},config:supervisorTimeout()};

handle_cast({reportFromArena,Arena,Value},State) ->
    Dict = State#state.reports,
    NewDict = dict:store(Arena,Value,Dict),
    case dict:size(NewDict) of
        3 ->
            logger:logGlobalStats(parallel,[{death,State#state.deathCounter},
                                            {fight,dict:fetch(fight,NewDict)},
                                            {reproduction,dict:fetch(reproduction,NewDict)},
                                            {migration,dict:fetch(migration,NewDict)}]),
            {noreply,State#state{reports = dict:new(), deathCounter = 0},config:supervisorTimeout()};
        _ ->
            {noreply,State#state{reports = NewDict},config:supervisorTimeout()}
    end;
handle_cast({go,ProblemSize},State) ->
    [spawn_link(agent,start,[self(),ProblemSize|State#state.arenas]) || _ <- lists:seq(1,config:populationSize())],
    timer:send_interval(config:writeInterval(),write),
    {noreply,State,config:supervisorTimeout()};

handle_cast(close,State) ->
    {stop,normal,State}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info({'EXIT',Pid,dying},State) ->
    NewAgentList = gb_trees:delete(Pid,State#state.agents),
    DeathCounter = State#state.deathCounter + 1,
    {noreply,State#state{deathCounter = DeathCounter, agents = NewAgentList},config:supervisorTimeout()};

handle_info({'EXIT',Pid,Reason},State) ->
    case lists:member(Pid,State#state.arenas) of
        true ->
            io:format("Error na arenie, zamykamy impreze na wyspie ~p~n",[self()]),
            exit(Reason);
        false ->
            io:format("Error w agencie, karawana jedzie dalej. Powod: ~p~n",[Reason]),
            NewAgentList = gb_trees:delete(Pid,State#state.agents),
            {noreply,State#state{agents = NewAgentList},config:supervisorTimeout()}
    end;

handle_info(write,State) ->
    {SumVar,MinVar,VarVar} = misc_util:diversity([Val || {_Key,Val} <- gb_trees:to_list(State#state.agents)]),
    logger:logLocalStats(parallel,fitness,State#state.best),
    logger:logLocalStats(parallel,population,gb_trees:size(State#state.agents)),
    logger:logLocalStats(parallel,stddevsum,SumVar),
    logger:logLocalStats(parallel,stddevmin,MinVar),
    logger:logLocalStats(parallel,stddevvar,VarVar),
    {noreply,State,config:supervisorTimeout()};

handle_info(timeout,State) ->
    {stop,timeout,State}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason,State) ->
    [Ring,Bar,Port] = State#state.arenas,
    port:close(Port),
    bar:close(Bar),
    ring:close(Ring).


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn,State,_Extra) ->
    {ok, State}.
