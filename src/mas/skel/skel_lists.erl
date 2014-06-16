%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(skel_lists).
-export([start/4]).

-include ("mas.hrl").

%% TODO przepisac counter na dicta
-record(counter, {fight = 0 :: non_neg_integer(),
                  reproduction = 0 :: non_neg_integer(),
                  migration = 0 :: non_neg_integer(),
                  death = 0 :: non_neg_integer()}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time,Islands,Topology,Path) ->
    topology:start_link(self(), Islands, Topology),
    skel_logger:start_link(Path),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    Environment = config:agent_env(),
    InitIslands = [Environment:initial_population() || _ <- lists:seq(1, Islands)],
    {_Time, _Result} = timer:tc(fun main/2, [InitIslands,Time]),
    topology:close(),
    skel_logger:close().
%%     io:format("Total time:   ~p s~nFitness:     ~p~n", [_Time / 1000000, _Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO dostosowac do nowego skela

%% TODO dodac generyczne statystyki z pliku emas.erl
%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
-spec main([island()], non_neg_integer()) -> float().
main(Islands, Time) ->
    Environment = config:agent_env(),
    EndTime = addMiliseconds(os:timestamp(), Time),
    Tag = {seq, fun(Island) ->
                        [{Environment:behaviour_function(Agent), Agent} || Agent <- Island] %% bylo behaviour no_mig
                end},
    Group = {seq, fun misc_util:groupBy/1},

    Log = {seq, fun(Island) ->
                        C = misc_util:countGroups(Island,#counter{}),
                        skel_logger:reportResult(fight,C#counter.fight),
                        skel_logger:reportResult(reproduce,C#counter.reproduction),
                        skel_logger:reportResult(death,C#counter.death),
                        Island
                end},

    %% Might be deleted
    Filter = {seq, fun(Island) ->  % Umieranie
                           lists:filter(fun({Atom,_}) ->
                                                Atom =/= death
                                        end,
                                        Island)
                   end},

    Work = {decomp,
                  [{farm,
                    [{seq,fun emas:meeting_function/1}], %% TODO substitute emas with agent_env!!!
                    4}],
                  fun identity/1,
                  fun identity/1},

    Shuffle = {seq, fun(Island) ->
                            misc_util:shuffle(lists:flatten(Island))
                    end},

    OneRun = {map,
              [{pipe, [Tag,
                       Group,
                       Log,
                       Filter,
                       Work,
                       Shuffle]}],
        %% TODO dopisac migracje przy uzyciu funkcji z sequential
%%               fun(Datachunk) ->
%%                       {_NrOfEmigrants, IslandsMigrated} = evolution:doMigrate(Datachunk),
%%                       IslandsMigrated
%%               end,
              fun identity/1,
              fun identity/1},

    [FinalIslands] = skel:do([{feedback,
                               [OneRun],
                               _While = fun(List) ->
                                                Fitness = lists:max([misc_util:result(Island) || Island <- List]),
                                                Population = lists:sum([length(Island) || Island <- List]),
                                                skel_logger:reportResult(fitness,Fitness),
                                                skel_logger:reportResult(population,Population),
                                                os:timestamp() < EndTime
                                        end}],
                             [Islands]),
%%     io_util:printSeq(FinalIslands),
    misc_util:result(lists:flatten(FinalIslands)).

%% sk_sendToWork({death, _}) ->
%%     erlang:error(neverShouldGetHere);
%% sk_sendToWork({fight, Agents}) ->
%%     evolution:doFight(Agents);
%% sk_sendToWork({reproduction, Agents}) ->
%%     evolution:doReproduce(Agents).

-spec addMiliseconds({integer(),integer(),integer()},integer()) -> {integer(),integer(),integer()}.
addMiliseconds({MegaSec, Sec, Milisec}, Time) ->
    {MegaSec,
     Sec + (Time div 1000),
     Milisec + (Time rem 1000)}.

-spec identity(Type) -> Type.
identity(Same) ->
    Same.

%% @doc Liczy kategorie (ile fights,deaths etc.) na wszystkich wyspach i dodaje do Counter.
%% -spec countAllIslands([list()], counter()) -> counter().
%% countAllIslands(GroupedIslands, Counter) ->
%%     CountedIslands = [misc_util:countGroups(I, #counter{}) || I <- GroupedIslands],
%%     lists:foldl(fun misc_util:addCounters/2, Counter, CountedIslands).
