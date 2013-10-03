%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy w modelu hybrydowym.

-module(hybrid_island).
-export([start/3, close/1, sendAgent/2]).

-record(counters,{fight = 0 :: non_neg_integer(),
                  reproduction = 0 :: non_neg_integer(),
                  migration = 0 :: non_neg_integer(),
                  death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type counters() :: #counters{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(Path::string(), N::integer(), ProblemSize::integer()) -> ok.
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia glowna
%% petle procesu.
start(Path,N,ProblemSize) ->
  misc_util:seedRandom(),
  IslandPath = filename:join([Path,"isl" ++ integer_to_list(N)]),
  FDs = io_util:prepareWriting(IslandPath),
  Agents = genetic:generatePopulation(ProblemSize),
  timer:send_after(config:writeInterval(),{write,-99999}),
  loop(Agents,FDs,#counters{}).

-spec close(pid()) -> {finish,pid()}.
close(Pid) ->
  Pid ! {finish,self()}.

-spec sendAgent(pid(),agent()) -> {agent,pid(),agent()}.
%% @doc Funkcja za pomoca ktorej mozna przesylac wyspie imigrantow.
%% Komunikacja asynchroniczna.
sendAgent(Pid,Agent) ->
   Pid ! {agent,self(),Agent}.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec loop([agent()],dict(),counters()) -> ok.
%% @doc Glowna petla procesu. Kazda iteracja powoduje wytworzenie kolejnej generacji.
loop(Agents,FDs,Counters) ->
  receive
    {write,Last} ->
      Fitness = case misc_util:result(Agents) of
                  islandEmpty -> Last;
                  X -> X
                end,
      io_util:write(dict:fetch(fitness,FDs),Fitness),
      io_util:write(dict:fetch(population,FDs),length(Agents)),
      io_util:write(dict:fetch(migration,FDs),Counters#counters.migration),
      io_util:write(dict:fetch(death,FDs),Counters#counters.death),
      io_util:write(dict:fetch(reproduction,FDs),Counters#counters.reproduction),
      io_util:write(dict:fetch(fight,FDs),Counters#counters.fight),
      io:format("Island ~p Fitness ~p Population ~p~n",[self(),misc_util:result(Agents),length(Agents)]),
      timer:send_after(config:writeInterval(),{write,Fitness}),
      loop(Agents,FDs,#counters{});
    {agent,_Pid,A} ->
      loop([A|Agents],FDs,Counters);
    {finish,_Pid} ->
      io_util:closeFiles(FDs),
      ok
  after 0 ->
    Groups = misc_util:groupBy([{misc_util:behavior(A),A} || A <- Agents ]),
    NewGroups = [evolution:sendToWork(G) || G <- Groups],
    NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),
    NewCounters = countGroups(Groups,#counters{}),
    loop(NewAgents,FDs,#counters{fight = Counters#counters.fight + NewCounters#counters.fight,
                                reproduction = Counters#counters.reproduction + NewCounters#counters.reproduction,
                                death = Counters#counters.death + NewCounters#counters.death,
                                migration = Counters#counters.migration + NewCounters#counters.migration})
  end.

-spec countGroups([tuple()],counters()) -> counters().
countGroups([],Counter) ->
  Counter;
countGroups([{death,AgentList}|Groups],Counter) ->
  countGroups(Groups,Counter#counters{death = length(AgentList)});
countGroups([{migration,AgentList}|Groups],Counter) ->
  countGroups(Groups,Counter#counters{migration = length(AgentList)});
countGroups([{fight,AgentList}|Groups],Counter) ->
  countGroups(Groups,Counter#counters{fight = length(AgentList)});
countGroups([{reproduction,AgentList}|Groups],Counter) ->
  countGroups(Groups,Counter#counters{reproduction = length(AgentList)}).