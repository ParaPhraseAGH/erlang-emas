-module (emas).
-behaviour(agent_env).

-export ([initial_population/0, behaviour_function/1, behaviours/0, meeting_functions/0]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.

-spec initial_population() -> [agent()].
initial_population() -> 
	genetic:generatePopulation(emas_config:problemSize()).

%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie jego energii.
-spec behaviour_function(agent()) -> death | reproduction | fight.
behaviour_function({_,_,0}) ->
    death;
behaviour_function({_, _, Energy}) ->
    case Energy > emas_config:reproductionThreshold() of
        true -> reproduction;
        false -> fight
    end.


behaviours() -> 
	[reproduction, death, fight].

meeting_functions() -> ok.
