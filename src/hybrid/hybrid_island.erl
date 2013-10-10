%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy w modelu hybrydowym.

-module(hybrid_island).
-export([start/1, close/1, sendAgent/2]).

-record(counter,{fight = 0 :: non_neg_integer(),
                  reproduction = 0 :: non_neg_integer(),
                  migration = 0 :: non_neg_integer(),
                  death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type counter() :: #counter{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(ProblemSize::integer()) -> ok.
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia glowna
%% petle procesu.
start(ProblemSize) ->
  misc_util:seedRandom(),
  Agents = genetic:generatePopulation(ProblemSize),
  timer:send_after(config:writeInterval(),{write,-99999}),
  loop(Agents,#counter{}).

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
-spec loop([agent()],counter()) -> ok.
%% @doc Glowna petla procesu. Kazda iteracja powoduje wytworzenie kolejnej generacji.
loop(Agents,Counters) ->
  receive
    {write,Last} ->
      Fitness = case misc_util:result(Agents) of
                  islandEmpty -> Last;
                  X -> X
                end,
      logger:logLocalStats(parallel,fitness,Fitness),
      logger:logLocalStats(parallel,population,length(Agents)),
      logger:logGlobalStats(parallel,{Counters#counter.death,Counters#counter.fight,Counters#counter.reproduction,Counters#counter.migration}),
      io:format("Island ~p Fitness ~p Population ~p~n",[self(),Fitness,length(Agents)]),
      timer:send_after(config:writeInterval(),{write,Fitness}),
      loop(Agents,#counter{});
    {agent,_Pid,A} ->
      loop([A|Agents],Counters);
    {finish,_Pid} ->
      ok
  after 0 ->
    Groups = misc_util:groupBy([{misc_util:behavior(A),A} || A <- Agents ]),
    NewGroups = [evolution:sendToWork(G) || G <- Groups],
    NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),
    NewCounters = misc_util:countGroups(Groups,#counter{}),
    loop(NewAgents,misc_util:addCounters(Counters,NewCounters))
  end.