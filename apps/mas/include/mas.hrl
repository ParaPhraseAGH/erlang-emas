-type island() :: [mas:agent()].

-type counter() :: dict:dict().

-type model() :: sequential | hybrid | concurrent | skel.

-type funstat() :: {Name::atom(), Map::fun(), Reduce::fun(), Value::term()}.

-record(config, {agent_env :: atom(),
                 topology :: atom(),
                 log_dir :: atom(),
                 islands :: pos_integer(),
                 population_size :: pos_integer(),
                 migration_probability :: float(),
                 write_interval :: pos_integer(),
                 skel_workers :: pos_integer(),
                 arena_timeout :: pos_integer()}).

-type config() :: #config{}.
