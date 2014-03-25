-module (emas).
-behaviour(agent_env).

-export ([initial_population/0, behaviour_function/1, behaviours/0, meeting_function/1]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type agent_behaviour() :: death | reproduction | fight | migration.

-spec initial_population() -> [agent()].
initial_population() -> 
    genetic:generatePopulation(emas_config:problemSize()).

%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie jego energii.
-spec behaviour_function(agent()) -> agent_behaviour().
% behaviour_function({no_migration, {_,_,0}}) -> death;
behaviour_function({_,_,0}) ->
    death;

% behaviour_function({_, _, Energy}) ->
%     case Energy > emas_config:reproductionThreshold() of
%         true -> reproduction;
%         false -> fight
%     end.

behaviour_function({_, _, Energy}) ->
    case random:uniform() < config:migrationProbability() of
        true -> migration;
        false -> case Energy > emas_config:reproductionThreshold() of
                     true -> reproduction;
                     false -> fight
                 end
    end.


-spec behaviours() -> [agent_behaviour()].
behaviours() -> 
    [reproduction, death, fight].

-spec meeting_function({agent_behaviour(), [agent()]}) -> [agent()].
meeting_function({death, _}) ->
    [];

meeting_function({reproduction, Agents}) ->
    lists:flatmap(fun evolution:doReproduce/1, evolution:optionalPairs(Agents,[]));
    
meeting_function({fight, Agents}) ->
    lists:flatmap(fun evolution:doFight/1, evolution:optionalPairs(Agents,[]));

meeting_function({migration, Agents}) ->
    %% TODO agenci w ogole nie migruja
    Agents;

meeting_function({_, _}) -> 
    erlang:error(unexpected_behaviour).
