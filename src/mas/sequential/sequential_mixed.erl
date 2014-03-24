%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny zawierajacy wszystkich agentow w jednej liscie.

-module(sequential_mixed).
-export([start/5, start/0, start/1]).

-record(counter,{fight = 0 :: non_neg_integer(),
                 reproduction = 0 :: non_neg_integer(),
                 migration = 0 :: non_neg_integer(),
                 death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type counter() :: #counter{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> ok.
start() ->
    file:make_dir("tmp"),
    start(40,5000,2,mesh,"tmp").

-spec start(list()) -> ok.
start([A,B,C,D,E]) ->
    start(list_to_integer(A),
          list_to_integer(B),
          list_to_integer(C),
          list_to_atom(D),E).

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path) ->
    io:format("{Model=sequential_mixed,ProblemSize=~p,Time=~p,Islands=~p,Topology=~p}~n",[ProblemSize,Time,Islands,Topology]),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    topology:start_link(Islands,Topology),
    logger:start_link({sequential,Islands},Path),
    Environment = config:agent_env(),
    Population = lists:append([[{X,Agent} || Agent <- Environment:initial_population()] || X <- lists:seq(1,Islands)]),
    timer:send_after(Time,theEnd),
    timer:send_after(config:writeInterval(),write),
    io:format("Population:~p~n",[length(Population)]), 
    {_Time,_Result} = timer:tc(fun loop/2, [Population,#counter{}]),
    topology:close(),
    logger:close().

%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
-spec loop([agent()],counter()) -> float().
loop(Population,Counter) ->
    Environment = config:agent_env(),
    receive
        write ->
            Islands = lists:sort(misc_util:groupBy(Population)),
            logger:logLocalStats(sequential,fitness,[misc_util:result(Agents) || {_,Agents} <- Islands]),
            logger:logLocalStats(sequential,population,[length(Agents) || {_,Agents} <- Islands]),
            logger:logGlobalStats(sequential,[{death,Counter#counter.death},
                                              {fight,Counter#counter.fight},
                                              {reproduction,Counter#counter.reproduction},
                                              {migration,Counter#counter.migration}]),
            PrintAgents = [A || {_,A} <- Population],
            Best = misc_util:result(PrintAgents),
            io:format("Best: ~p  Energy:~p~n",[Best,io_util:sumEnergy(PrintAgents)]),
            timer:send_after(config:writeInterval(),write),
            loop(Population,#counter{});
        theEnd ->
            misc_util:result([A || {_,A} <- Population])
    after 0 ->
            Groups = misc_util:groupBy([{Environment:behaviour_function(HomeAgent),HomeAgent} || HomeAgent <- Population]),         % Groups = [{death,[{Home1,Agent1},{H2,A2}]},{fight,[...]}]
            {DeathMigration,FightReproduction} = lists:partition(fun({Atom,_}) -> lists:member(Atom,[death,migration]) end,Groups),
            DeadAndMigrated = [Environment:meeting_function(G) || G <- DeathMigration],
            FRRegrouped = [{Job,misc_util:groupBy(AgentList)} || {Job,AgentList} <- FightReproduction],     % FRRegrouped = [{fight,[{H1,[A1,A2]},{H2,[A3,A5]},...]},{reproduction,[...]}]
            Fighters = case lists:keyfind(fight,1,FRRegrouped) of                                           % w przyszlosci mozna zrobic fighterow i reproduktowcow w jednej liscie i operowac na list comprehensions
                           {fight,FAgents} -> FAgents;
                           false -> []
                       end,                                                                                            % Fighters = [{H1,[A1,A2]},{H2,[A3,A5]},...]
            Reproducers = case lists:keyfind(reproduction,1,FRRegrouped) of
                              {reproduction,RAgents} -> RAgents;
                              false -> []
                          end,                                                                                            % Reproducers = [{H1,[A1,A2]},{H2,[A3,A5]},...]
            AfterFights = [{Home,Environment:meeting_function({fight,AgentList})} || {Home,AgentList} <- Fighters], % AfterFights = [{H1,[A1',A2']},{H2,[A3',A5']},...]
            AfterReproductions = [{Home,Environment:meeting_function({reproduction,AgentList})} || {Home,AgentList} <- Reproducers],
            AfterWork = AfterFights ++ AfterReproductions,
            Degrouped = [[{Home,A} || A <- List] || {Home,List} <- AfterWork],                              % Degrouped = [[{H1,A1'},{H1,A2'}],[{H2,A3'}...]
            NewAgents = lists:flatten([DeadAndMigrated|Degrouped]),
                                                %io:format("Population: ~p~n",[NewAgents]),
            NewCounter = misc_util:countGroups(Groups,#counter{}),
            loop(misc_util:shuffle(NewAgents),misc_util:addCounters(Counter,NewCounter))
    end.