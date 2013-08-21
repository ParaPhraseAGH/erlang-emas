%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy.

-module(island).
-export([proces/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec proces() -> loop(List)
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia
%% petle, w ktorej porusza sie proces.
proces(Instancja,N) ->
  random:seed(erlang:now()),
  FDs = emas_util:prepareWriting(Instancja ++ "\\" ++ integer_to_list(N)),
  Solutions = [genetic:solution() || _ <- lists:seq(1, config:populationSize())],
  Agents = [ {S, genetic:evaluation(S), config:initialEnergy()} || S <- Solutions],
  loop(Agents,FDs).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec loop(List1) -> loop(List2)
%% @doc Glowna petla procesu. Kazda iteracja powoduje obliczenie
%% kolejnego wyniku i opcjonalnie wyslanie go do supervisora.
loop(Agents,FDs) ->
  receive
    {agent,_Pid,A} ->
      loop([A|Agents],FDs);
    {finish,_Pid} ->
      emas_util:closeFiles(FDs)
  after 0 ->
    Groups = emas_util:groupBy(fun emas_util:behavior/1, Agents),
    NewGroups = [sendToWork(G) || G <- Groups],
    NewAgents = emas_util:shuffle(lists:flatten(NewGroups)),
    Result = emas_util:result(NewAgents),
    emas_util:write(dict:fetch(fitness,FDs),Result),
    emas_util:write(dict:fetch(population,FDs),length(NewAgents)),
    if Result /= islandEmpty ->
      whereis(supervisor) ! {result,Result};
      Result == islandEmpty ->
        donothing
    end,
    %emas_util:print(Result,Groups),
    loop(NewAgents,FDs)
  end.

%% @spec sendToWork({atom(),List1}) -> List2
%% @doc Funkcja dostaje atom precyzujacy klase agentow i ich liste,
%% a nastepnie wykonuje odpowiednie operacje dla kazdej z klas.
%% Funkcja zwraca liste agentow po przetworzeniu.
sendToWork({death, _}) ->
  [];
sendToWork({fight, Agents}) ->
  lists:flatmap(fun doFight/1, emas_util:optionalPairs(Agents));
sendToWork({reproduction,Agents}) ->
  lists:flatmap(fun doReproduce/1, emas_util:optionalPairs(Agents));
sendToWork({migration,Agents}) ->
  case Agents of
    [] -> [];
    [H|T] ->
      whereis(supervisor) ! {agent,self(),H},
      sendToWork({migration,T})
  end.

%% @spec doFight({Agent1}) -> [Agent2]
%% @doc Funkcja implementujaca logike "walki" pojedynczego agenta.
%% Zwracany jest ten sam agent w liscie.
doFight({A}) -> [A];
%% @spec doFight({Agent1,Agent2}) -> [Agent3,Agent4]
%% @doc Funkcja implementujaca logike walki dwoch agentow.
%% Zwracana jest lista dwoch przetworzonych agentow.
doFight({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  AtoBtransfer =
    if EvA < EvB -> erlang:min(config:fightTransfer(), EnA);
      EvA >= EvB -> -erlang:min(config:fightTransfer(), EnB)
    end,
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, EnB + AtoBtransfer}].

%% @spec doReproduce({Agent1}) -> List
%% @doc Funkcja implementujaca logike reprodukcji pojedynczego agenta.
%% Zwracana jest dwojka agentow w liscie.
doReproduce({{SolA, EvA, EnA}}) ->
  SolB = genetic:reproduction(SolA),
  EvB = genetic:evaluation(SolB),
  AtoBtransfer = erlang:min(config:reproductionTransfer(), EnA),
  [{SolA, EvA, EnA - AtoBtransfer}, {SolB, EvB, AtoBtransfer}];
%% @spec doReproduce({Agent1,Agent2}) -> [Agent3,Agent4,Agent5,Agent6]
%% @doc Funkcja implementujaca logike reprodukcji dwoch agentow.
%% Zwracanych jest czterech agentow w liscie.
doReproduce({{SolA, EvA, EnA}, {SolB, EvB, EnB}}) ->
  [SolC, SolD] = genetic:reproduction(SolA, SolB),
  [EvC, EvD] = [ genetic:evaluation(S) || S <- [SolC, SolD] ],
  [AtoCTransfer, BtoDTransfer] = [ erlang:min(config:reproductionTransfer(), E) || E <- [EnA, EnB] ],
  [{SolA, EvA, EnA - AtoCTransfer}, {SolB, EvB, EnB - BtoDTransfer}, {SolC, EvC, AtoCTransfer}, {SolD, EvD, BtoDTransfer}].