%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny zawierajacy wszystkich agentow w jednej liscie.

-module(sequential_mixed).
-export([start/5, start/0, start/1]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> ok.
start() ->
  sequential:start(fun start/5).

-spec start(list()) -> ok.
start(Args) ->
  sequential:start(Args,fun start/5).

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path) ->
  sequential:start(ProblemSize,Time,Islands,Topology,Path,fun init/5).

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec init(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> {float(),[dict()]}.
%% @doc Funkcja tworzaca odpowiednia ilosc wysp i przechodzaca do glownej petli.
%% Zwracany jest koncowy wynik.
init(ProblemSize,Time,IslandsNr,Topology,Path) ->
  Population = lists:append([[{X,genetic:generateAgent(ProblemSize)} || _ <- lists:seq(1,config:populationSize())] || X <- lists:seq(1,IslandsNr)]),
  sequential:init(Time,IslandsNr,Topology,Path,Population,fun loop/2).

-spec loop([agent()],[dict()]) -> {float(),[dict()]}.
%% @doc Glowa petla programu. Każda iteracja powoduje ewolucję nowej generacji osobnikow.
loop(Population,FDs) ->
  receive
    write ->
      Islands = lists:sort(misc_util:groupBy(Population)),
      io_util:writeIslands(FDs,[Agents || {_,Agents} <- Islands]),
      PrintAgents = [A || {_,A} <- Population],
      io:format("Best: ~p  Energy:~p~n",[misc_util:result(PrintAgents),io_util:sumEnergy(PrintAgents)]),
      timer:send_after(config:writeInterval(),write),
      loop(Population,FDs);
    theEnd ->
      Best = misc_util:result([A || {_,A} <- Population]),
      {Best,FDs}
  after 0 ->
    Groups = misc_util:groupBy([{misc_util:behavior(Agent),Agent} || Agent <- Population]),
    {DeathMigration,FightReproduction} = lists:partition(fun({Atom,_}) -> lists:member(Atom,[death,migration]) end,Groups),
    DeadAndMigrated = [evolution:sendToWork(G) || G <- DeathMigration],
    FRRegrouped = [{Job,misc_util:groupBy(AgentList)} || {Job,AgentList} <- FightReproduction],
    Fighters = case lists:keyfind(fight,1,FRRegrouped) of
      {fight,FAgents} -> FAgents;
      false -> []
    end,
    Reproducers = case lists:keyfind(reproduction,1,FRRegrouped) of
      {reproduction,RAgents} -> RAgents;
      false -> []
    end,
    AfterWork = lists:append([{Home,evolution:sendToWork({fight,AgentList})} || {Home,AgentList} <- Fighters],
      [{Home,evolution:sendToWork({reproduction,AgentList})} || {Home,AgentList} <- Reproducers]),
    Degrouped = [[{Home,A} || A <- List] || {Home,List} <- AfterWork],
    NewAgents = misc_util:shuffle(lists:flatten(lists:append(DeadAndMigrated,Degrouped))),
    %io:format("Population: ~p~n",[NewAgents]),
    loop(NewAgents,FDs)
  end.

