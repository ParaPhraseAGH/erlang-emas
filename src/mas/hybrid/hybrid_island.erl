%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy w modelu hybrydowym.

-module(hybrid_island).
-export([start/0, close/1, sendAgent/2]).

-include ("mas.hrl").

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
    timer:send_after(config:writeInterval(),write),
    loop(Agents,dict:new(),Environment:stats()).

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
-spec loop([agent()],counter(),[tuple()]) -> ok.
loop(Agents,Counter,Stats) ->
    Environment = config:agent_env(),
    receive
        write ->
            {fitness,_,Fitness} = lists:keyfind(fitness,1,Stats),
            {energy,_,Energy} = lists:keyfind(energy,1,Stats),
            io:format("Energy: ~p~n",[Energy]),
            logger:logLocalStats(parallel,fitness,Fitness),
            logger:logLocalStats(parallel,population,length(Agents)),
            logger:logGlobalStats(parallel,Counter),
            timer:send_after(config:writeInterval(),write),
            Environment = config:agent_env(),
            loop(Agents,misc_util:createNewCounter(),Environment:stats());
        {agent,_Pid,A} ->
            loop([A|Agents],Counter,Stats);
        {finish,_Pid} ->
            ok
    after 0 ->
            NewStats = countStats(Agents,Stats),
            Groups = misc_util:groupBy([{Environment:behaviour_function(A),A} || A <- Agents ]),
            NewGroups = [misc_util:meeting_proxy(G, hybrid) || G <- Groups],
            NewAgents = misc_util:shuffle(lists:flatten(NewGroups)),
            NewCounter = misc_util:countInteractions([Groups],Counter),
            loop(NewAgents,NewCounter,NewStats)
    end.


countStats(_,[]) ->
    [];

countStats(Agents,[{Stat,Fun,Acc0}|T]) ->
    NewAcc = lists:foldl(Fun,Acc0,Agents),
    [{Stat,Fun,NewAcc} | countStats(Agents,T)].
