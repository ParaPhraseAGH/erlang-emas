-module (agent_env).

-include ("mas.hrl").

-callback initial_population() -> [agent()].

-callback behaviour_function(agent()) -> agent_behaviour().

-callback behaviours() -> [agent_behaviour()].

-callback meeting_function({agent_behaviour(), [agent()]}) -> [agent()].