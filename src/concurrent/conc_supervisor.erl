%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul supervisora wyspy w modelu wspolbieznym.

-module(conc_supervisor).
-behaviour(gen_server).

%% API
-export([start/2, newAgent/2, sendAgents/2, unlinkAgent/3, linkAgent/3, reportFromArena/3, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(King::pid(), ProblemSize::pos_integer()) -> pid().
start(King,ProblemSize) ->
    {ok,Pid} = gen_server:start(?MODULE,[King,ProblemSize],[]),
    Pid.

-spec newAgent(pid(),agent()) -> ok.
newAgent(Pid,Agent) ->
    gen_server:cast(Pid,{newAgent,Agent}).

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
                population = 0 :: pos_integer(),
                diversity :: {Mean::[float()],StdDev::[float()],DevSum::float(),DevMin::float()},
                deathCounter = 0 :: non_neg_integer(),
                reports = dict:new() :: dict(),
                arenas :: [pid()]}).
-type state() :: #state{}.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([King,ProblemSize]) ->
    misc_util:seedRandom(),
    process_flag(trap_exit, true),
    {ok,Ring} = ring:start_link(self()),
    {ok,Bar} = bar:start_link(self()),
    {ok,Port} = port:start_link(self(),King),
    Arenas = [Ring,Bar,Port],
    io_util:printArenas(Arenas),
    [spawn_link(agent,start,[self(),ProblemSize|Arenas]) || _ <- lists:seq(1,config:populationSize())],
    timer:send_interval(config:writeInterval(),write),
    Vector = lists:duplicate(ProblemSize,0.0),
    {ok,#state{arenas = Arenas, diversity = {Vector,Vector,0.0,0.0}},config:supervisorTimeout()}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call({emigrant,AgentPid,{Solution,_,_}},_From,State) ->
    erlang:unlink(AgentPid),
    NewPopulation = State#state.population - 1,
    {Mean,StdDev,_,_} = State#state.diversity,
    {Sum,Min,NewMean,NewStddev} = misc_util:concurrentDiversity(Solution,delete,NewPopulation,Mean,StdDev),
    {reply,ok,State#state{population = NewPopulation, diversity = {NewMean,NewStddev,Sum,Min}}};

handle_call({immigrant,AgentFrom,Agent},_From,State) ->
    {AgentPid,_} = AgentFrom,
    erlang:link(AgentPid),
    gen_server:reply(AgentFrom,State#state.arenas),
    newAgent(self(),Agent),
    {reply,ok,State}.


-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({newAgents,AgentList},State) ->
    [spawn_link(agent,start,[A|State#state.arenas]) || A <- AgentList],
    Result = misc_util:result(AgentList),
    {{NewMean,NewM,NewSum,NewMin},NewPopulation} = lists:foldl(fun({Solution,_,_},{{Mean,M,_,_},Population}) ->
                                                                       {Sum,Min,MidMean,MidM} = misc_util:concurrentDiversity(Solution,add,Population+1,Mean,M),
                                                                       {{MidMean,MidM,Sum,Min},Population+1}
                                                               end,{State#state.diversity,State#state.population},AgentList),
    {noreply,State#state{best = lists:max([Result,State#state.best]),
                         population = NewPopulation,
                         diversity = {NewMean,NewM,NewSum,NewMin}},config:supervisorTimeout()};

handle_cast({newAgent,{Solution,_,_}},State) ->
    N = State#state.population + 1,
    {Mean,StdDev,_,_} = State#state.diversity,
    {Sum,Min,NewMean,NewStddev} = misc_util:concurrentDiversity(Solution,add,N,Mean,StdDev),
    {noreply,State#state{population = N,diversity = {NewMean,NewStddev,Sum,Min}},config:supervisorTimeout()};

handle_cast({reportFromArena,Arena,Value},State) ->
    Dict = State#state.reports,
    NewDict = dict:store(Arena,Value,Dict),
    case dict:size(NewDict) of
        3 ->
            logger:logGlobalStats(parallel,{State#state.deathCounter,dict:fetch(fight,NewDict),dict:fetch(reproduction,NewDict),dict:fetch(migration,NewDict)}),
            {noreply,State#state{reports = dict:new(), deathCounter = 0},config:supervisorTimeout()};
        _ ->
            {noreply,State#state{reports = NewDict},config:supervisorTimeout()}
    end;

handle_cast(close,State) ->
    {stop,normal,State}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info({'EXIT',_,{dying,{Solution,_,_}}},State) ->
    Population = State#state.population - 1,
    {Mean,StdDev,_,_} = State#state.diversity,
    {Sum,Min,NewMean,NewStddev} = misc_util:concurrentDiversity(Solution,delete,Population,Mean,StdDev),
    DeathCounter = State#state.deathCounter,
    {noreply,State#state{population = Population, deathCounter = DeathCounter + 1, diversity = {NewMean,NewStddev,Sum,Min}},config:supervisorTimeout()};

handle_info({'EXIT',Pid,Reason},State) ->
    case lists:member(Pid,State#state.arenas) of
        true ->
            io:format("Error w arenie, zamykamy impreze na wyspie ~p~n",[self()]),
            exit(Reason);
        false ->
            io:format("Error w agencie, karawana jedzie dalej~n"),
            Population = State#state.population,
            {noreply,State#state{population = Population - 1},config:supervisorTimeout()}
    end;

handle_info(write,State) ->
    Fitness = State#state.best,
    Population = State#state.population,
    {_,_,StdSum,StdMin} = State#state.diversity,
    logger:logLocalStats(parallel,fitness,Fitness),
    logger:logLocalStats(parallel,population,Population),
    logger:logLocalStats(parallel,stddevsum,StdSum),
    logger:logLocalStats(parallel,stddevmin,StdMin),
    %%     io:format("Island ~p Fitness ~p Population ~p~n",[self(),Fitness,Population]),
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