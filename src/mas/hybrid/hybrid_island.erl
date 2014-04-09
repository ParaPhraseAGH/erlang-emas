%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy w modelu hybrydowym.

-module(hybrid_island).
-export([start/0, close/1, sendAgent/2]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type counter() :: dict().

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Funkcja generujaca dane poczatkowe, ktora pod koniec uruchamia glowna
%% petle procesu.
-spec start() -> ok.
start() ->
    misc_util:seedRandom(),
    Environment = config:agent_env(),
    Agents = Environment:initial_population(),
    timer:send_after(config:writeInterval(),{write,-99999}),
    loop(Agents,dict:new()).

-spec close(pid()) -> {finish,pid()}.
close(Pid) ->
    Pid ! {finish,self()}.

%% @doc Funkcja za pomoca ktorej mozna przesylac wyspie imigrantow.
%% Komunikacja asynchroniczna.
-spec sendAgent(pid(),agent()) -> {agent,pid(),agent()}.
sendAgent(Pid,Agent) ->
    Pid ! {agent,self(),Agent}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc Glowna petla procesu. Kazda iteracja powoduje wytworzenie kolejnej generacji.
-spec loop([agent()],counter()) -> ok.
loop(Agents,Counter) ->
    Environment = config:agent_env(),
    receive
        {write,Last} ->
            Fitness = case misc_util:result(Agents) of
                          islandEmpty -> Last;
                          X -> X
                      end,
            logger:logLocalStats(parallel,fitness,Fitness),
            logger:logLocalStats(parallel,population,length(Agents)),
            logger:logGlobalStats(parallel,Counter),
            timer:send_after(config:writeInterval(),{write,Fitness}),
            loop(Agents,misc_util:createNewCounter());
        {agent,_Pid,A} ->
            loop([A|Agents],Counter);
        {finish,_Pid} ->
            ok
    after 0 ->
            Groups = misc_util:groupBy([{Environment:behaviour_function(A),A} || A <- Agents ]),
            NewGroups = [misc_util:meeting_proxy(G, hybrid) || G <- Groups],
            NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),
            NewCounter = misc_util:countInteractions([Groups],Counter),
            loop(NewAgents,NewCounter)
    end.