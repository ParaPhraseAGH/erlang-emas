
-type solution() :: [float()].
-type agent() :: {Solution::solution(), Fitness::float(), Energy::pos_integer()}.
-type agent_behaviour() :: death | reproduction | fight | migration.