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
  FDs = sequential:init(Time,IslandsNr,Topology,Path),
  loop(Population,FDs).

-spec loop([agent()],[dict()]) -> {float(),[dict()]}.
%% @doc Glowa petla programu. Każda iteracja powoduje ewolucję nowej generacji osobnikow.
loop(Population,FDs) ->
  receive
    {write,Last} ->
      Islands = lists:sort(misc_util:groupBy(Population)),
      io_util:writeIslands(FDs,[Agents || {_,Agents} <- Islands],Last),
      PrintAgents = [A || {_,A} <- Population],
      Best = misc_util:result(PrintAgents),
      io:format("Best: ~p  Energy:~p~n",[Best,io_util:sumEnergy(PrintAgents)]),
      timer:send_after(config:writeInterval(),{write,Best}),
      loop(Population,FDs);
    theEnd ->
      Best = misc_util:result([A || {_,A} <- Population]),
      {Best,FDs}
  after 0 ->
    Groups = misc_util:groupBy([{misc_util:behavior(Agent),Agent} || Agent <- Population]),         % Groups = [{death,[{Home1,Agent1},{H2,A2}]},{fight,[...]}]
    {DeathMigration,FightReproduction} = lists:partition(fun({Atom,_}) -> lists:member(Atom,[death,migration]) end,Groups),
    DeadAndMigrated = [evolution:sendToWork(G) || G <- DeathMigration],
    FRRegrouped = [{Job,misc_util:groupBy(AgentList)} || {Job,AgentList} <- FightReproduction],     % FRRegrouped = [{fight,[{H1,[A1,A2]},{H2,[A3,A5]},...]},{reproduction,[...]}]
    Fighters = case lists:keyfind(fight,1,FRRegrouped) of                                           % w przyszlosci mozna zrobic fighterow i reproduktowcow w jednej liscie i operowac na list comprehensions
      {fight,FAgents} -> FAgents;
      false -> []
    end,                                                                                            % Fighters = [{H1,[A1,A2]},{H2,[A3,A5]},...]
    Reproducers = case lists:keyfind(reproduction,1,FRRegrouped) of
      {reproduction,RAgents} -> RAgents;
      false -> []
    end,                                                                                            % Reproducers = [{H1,[A1,A2]},{H2,[A3,A5]},...]
    AfterFights = [{Home,evolution:sendToWork({fight,AgentList})} || {Home,AgentList} <- Fighters], % AfterFights = [{H1,[A1',A2']},{H2,[A3',A5']},...]
    AfterReproductions = [{Home,evolution:sendToWork({reproduction,AgentList})} || {Home,AgentList} <- Reproducers],
    AfterWork = lists:append(AfterFights,AfterReproductions),
    Degrouped = [[{Home,A} || A <- List] || {Home,List} <- AfterWork],                              % Degrouped = [[{H1,A1'},{H1,A2'}],[{H2,A3'}...]
    NewAgents = lists:flatten([DeadAndMigrated|Degrouped]),
    %io:format("Population: ~p~n",[NewAgents]),
    loop(misc_util:shuffle(NewAgents),FDs)
  end.