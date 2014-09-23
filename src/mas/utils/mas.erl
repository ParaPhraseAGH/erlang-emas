%% @doc This module starts the mas framework with given environment, model and parameters

-module (mas).
-export ([start/5]).

-include ("mas.hrl").

-spec start(atom(), model(), pos_integer(), sim_params(), [tuple()]) -> ok.
start(Module, Model, Time, EmasParams, Options) ->
    {ok, ConfigFromFile} = file:consult("etc/mas.config"),
    ConfigWithEnv = [{agent_env,Module}|ConfigFromFile],
    OverwrittenConfig = overwrite(Options, ConfigWithEnv),
    ConfigRecord = proplist_to_record(OverwrittenConfig),
    %%     Model:start(Time,Islands,Topology,Path),
    ok.


%% @doc Overwrites the configuration parameters read from a file
%%      with the options with which the application was run
-spec overwrite([tuple()],[tuple()]) -> [tuple()].
overwrite([], Config) ->
    Config;

overwrite([{Key, Val}|OtherOptions], Config) ->
    overwrite(OtherOptions, lists:keyreplace(Key, 1, Config, {Key, Val})).


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