%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(skel_lists).
-export([start/4]).

-include ("mas.hrl").

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

%% TODO add generic statistics (funstats)
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
                        Counter = misc_util:createNewCounter(),
                        Counts = misc_util:add_interactions_to_counter(Island,Counter),
                        skel_logger:reportResult(fight, dict:fetch(fight,Counts)),
                        skel_logger:reportResult(reproduce, dict:fetch(reproduction,Counts)),
                        skel_logger:reportResult(death, dict:fetch(death,Counts)),
                        %% TODO migration is not counted
                        Island
                end},

    Work = {map,
            [{seq,fun Environment:meeting_function/1}],
            4},

    Shuffle = {seq, fun(Island) ->
                            misc_util:shuffle(lists:flatten(Island))
                    end},

    OneRun = {map,
              [{pipe, [Tag,
                       Group,
                       Log,
                       Work,
                       Shuffle]}]},

    Migration = {seq, fun(AllIslands) ->
                              {_NrOfEmigrants, IslandsMigrated} = doMigrate(AllIslands),
                              IslandsMigrated
                      end},

    [FinalIslands] = skel:do([{feedback,
                               [OneRun,
                                Migration],
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

-spec addMiliseconds({integer(),integer(),integer()},integer()) -> {integer(),integer(),integer()}.
addMiliseconds({MegaSec, Sec, Milisec}, Time) ->
    {MegaSec,
     Sec + (Time div 1000),
     Milisec + (Time rem 1000)}.

%% -spec identity(Type) -> Type.
%% identity(Same) ->
%%     Same.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja dokonujaca migracji. Najpierw z kazdej wyspy pobierana jest statystyczna
%% liczba agentow, ktorzy powinni ulec migracji. Dla kazdej grupy emigrantow wyznaczana jest wyspa docelowa
%% i sa oni do niej dopisywani. Zwracana jest lista wysp po dokonanej migracji.
-spec doMigrate([island()]) -> {non_neg_integer(), [island()]}.
doMigrate(Islands) ->
    {Gathered, NewIslands} = gather(Islands, [], []),
    {length(Gathered), append(Gathered, lists:reverse(NewIslands))}.

%% @doc Funkcja dla kazdej grupy emigrantow z listy wyznacza wyspe docelowa oraz dokleja ich do tamtejszej populacji.
-spec append([{[agent()], integer()}], [island()]) -> [island()].
append([], Islands) -> Islands;
append([{Immigrants, From}|T], Islands) ->
    Destination = topology:getDestination(From),
    NewIslands = misc_util:mapIndex(Immigrants, Destination, Islands, fun lists:append/2),
    append(T, NewIslands).

%% @doc Funkcja wyznacza ile srednio agentow z danej populacji powinno emigrowac i przesuwa ich do specjalnej listy emigrantow.
%% Zwracana jest wyznaczona lista emigrantow oraz uszczuplona lista wysp.
-spec gather([island()], [island()], [{[agent()], integer()}]) -> {[{[agent()], integer()}], [island()]}.
gather([], Islands, Emigrants) ->
    {Emigrants, Islands};
gather([I|T], Acc, Emigrants) ->
    N = misc_util:averageNumber(emas_config:migrationProbability(), I),
    case N of
        0 ->
            gather(T, [I|Acc], Emigrants);
        _ ->
            {NewEmigrants, NewIsland} = lists:split(N, I),
            gather(T, [NewIsland|Acc], [{NewEmigrants, length(Acc) + 1}|Emigrants])
    end.
