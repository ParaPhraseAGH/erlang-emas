%% @doc The module contains the definition of agent environment operator callbacks to be implemented.

-module (mas_agent_env).

-include ("mas.hrl").

-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().

-callback start(model(), pos_integer()) -> ok.

-callback initial_agent(sim_params()) -> [agent()].

-callback behaviour_function(agent(), sim_params()) -> agent_behaviour().

-callback behaviours() -> [agent_behaviour()].

-callback meeting_function({agent_behaviour(), [agent()]}, sim_params()) -> [agent()].

-callback stats() -> [funstat()].