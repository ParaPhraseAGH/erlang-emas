%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul supervisora wyspy w modelu wspolbieznym.

-module(conc_supervisor).
-behaviour(gen_server).

%% API
-export([start/0, go/2, getArenas/1, newAgent/2, reportFromArena/3, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> pid().
start() ->
    {ok,Pid} = gen_server:start(?MODULE,[],[]),
    Pid.

-spec newAgent(pid(),agent()) -> ok.
newAgent(Pid,Agent) ->
    gen_server:cast(Pid,{newAgent,self(),Agent}).

-spec go(pid(),non_neg_integer()) -> ok.
go(Pid,ProblemSize) ->
    gen_server:cast(Pid,{go,ProblemSize}).

-spec getArenas(pid()) -> [pid()].
getArenas(Pid) ->
    gen_server:call(Pid,getArenas).

-spec reportFromArena(pid(),fight | reproduction | migration | death, term()) -> ok.
reportFromArena(Pid,Arena,Value) ->
    gen_server:cast(Pid,{reportFromArena,Arena,Value}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid,close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {agents = gb_trees:empty() :: gb_tree(),
                reports = dict:new() :: dict(),
                db4b = [] :: [pid()],
                arenas :: [pid()]}).
-type state() :: #state{}.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([]) ->
    misc_util:seedRandom(),
    {ok,Cemetery} = cemetery:start_link(self()),
    {ok,Ring} = ring:start_link(self()),
    {ok,Bar} = bar:start_link(self()),
    {ok,Port} = port:start_link(self()),
    Arenas = [Ring,Bar,Port,Cemetery],
    bar:giveArenas(Bar,Arenas),
    port:giveArenas(Port,Arenas),
    io_util:printArenas(Arenas),
    {ok,#state{arenas = Arenas}}.


-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call(getArenas,_From,State) ->
    {reply,State#state.arenas,State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({reportFromArena,Arena,Value},State) ->
    Dict = State#state.reports,
%%     error = dict:find(Arena,Dict), % debug - tu wyskakuje error!
    NewDict = dict:store(Arena,Value,Dict),
    case dict:size(NewDict) of
        4 ->
            {Agents,Db4b} = logStats(NewDict,State),
            {noreply,State#state{reports = dict:new(), agents = Agents, db4b = Db4b},config:supervisorTimeout()};
        _ ->
            {noreply,State#state{reports = NewDict}}
    end;

handle_cast({newAgent,Pid,Agent},State) ->
    NewPopulation = gb_trees:insert(Pid,Agent,State#state.agents),
    {noreply,State#state{agents = NewPopulation}};

handle_cast({go,ProblemSize},State) ->
    [spawn(agent,start,[self(),ProblemSize|State#state.arenas]) || _ <- lists:seq(1,config:populationSize())],
    {noreply,State};

handle_cast(close,State) ->
    {stop,normal,State}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout,State) ->
    {stop,timeout,State}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason,State) ->
    [Ring,Bar,Port,Cemetery] = State#state.arenas,
    port:close(Port),
    bar:close(Bar),
    ring:close(Ring),
    cemetery:close(Cemetery).


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn,State,_Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

logStats(Dict,State) ->
    Deaths = dict:fetch(death,Dict),
    {Emigrations,Immigrations} = dict:fetch(migration,Dict),
    {_Best,Reproductions} = dict:fetch(reproduction,Dict),
    Add1 = Reproductions ++ Immigrations,
    Del1 = Deaths ++ Emigrations,
    Db4b = State#state.db4b -- [Pid || {Pid,_} <- Add1],
    Add2 = lists:foldl(fun(Pid,List) ->
                               lists:keydelete(Pid,1,List)
                       end,Add1,State#state.db4b),
    Add3 = lists:foldl(fun(Pid,List) ->
                               lists:keydelete(Pid,1,List)
                       end,Add2,Del1),
    Del3 = Del1 -- [X || {X,_} <- Add2],
    case lists:sort([P || {P,_} <- Add3]) == lists:usort([P || {P,_} <- Add3]) of
        true ->
            nothing;
        false ->
            error("DUPLICATE!")
    end,
    true = lists:all(fun({Pid,_}) ->
                             not gb_trees:is_defined(Pid,State#state.agents)
                     end,Add3),
    Population1 = lists:foldl(fun({Key,Val},Tree) ->
                                      gb_trees:insert(Key,Val,Tree) % insert czy enter?
                              end, State#state.agents, Add3),
    {NewAgents,NewDb4b} = lists:foldl(fun(Pid,{Tree,TMPdb4b}) ->
                                              case gb_trees:is_defined(Pid,Tree) of
                                                  false ->
                                                      {Tree,[Pid|TMPdb4b]};
                                                  true ->
                                                      {gb_trees:delete(Pid,Tree),TMPdb4b}
                                              end
                                      end, {Population1,Db4b}, Del3),
    {SumVar,MinVar,VarVar} = misc_util:diversity([Val || {_Key,Val} <- gb_trees:to_list(NewAgents)]),
%%     logger:logLocalStats(parallel,fitness,Best),
%%     logger:logLocalStats(parallel,population,gb_trees:size(NewAgents)),
%%     logger:logLocalStats(parallel,stddevsum,SumVar),
%%     logger:logLocalStats(parallel,stddevmin,MinVar),
%%     logger:logLocalStats(parallel,stddevvar,VarVar),
%%     logger:logGlobalStats(parallel,[{death,length(Deaths)},
%%                                     {fight,dict:fetch(fight,Dict)},
%%                                     {reproduction,length(Reproductions)},
%%                                     {migration,length(Emigrations)}]),
    {gb_trees:balance(NewAgents),NewDb4b}.
