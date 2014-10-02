%% @doc This module starts the mas framework with given environment, model and parameters

-module (mas).
-export ([start/5]).

-include ("mas.hrl").

-spec start(atom(), model(), pos_integer(), sim_params(), [tuple()]) -> ok.
start(Module, Model, Time, SimParams, Options) ->
    {ok, ConfigFromFile} = file:consult("etc/mas.config"),
    ConfigWithEnv = [{agent_env,Module}|ConfigFromFile],
    OverwrittenConfig = misc_util:overwrite_options(Options, ConfigWithEnv),
    ConfigRecord = proplist_to_record(OverwrittenConfig),
    Model:start(Time, SimParams, ConfigRecord),
    ok.


%% @doc Transform a proplist with config properties to a record
-spec proplist_to_record([tuple()]) -> config().
proplist_to_record(Proplist) ->
    Dict = dict:from_list(Proplist),
    #config{agent_env = dict:fetch(agent_env,Dict),
            topology = dict:fetch(topology,Dict),
            log_dir = dict:fetch(log_dir,Dict),
            islands = dict:fetch(islands,Dict),
            population_size = dict:fetch(population_size,Dict),
            write_interval = dict:fetch(write_interval,Dict),
            arena_timeout = dict:fetch(arena_timeout,Dict)}.