%% @doc This module starts the mas framework with given environment, model and parameters

-module (mas).
-export ([start/5]).

-include ("mas.hrl").

-define(LOAD(Prop, Dict), Prop = dict:fetch(Prop,Dict)).

-spec start(atom(), model(), pos_integer(), sim_params(), [tuple()]) -> ok.
start(Module, Model, Time, SP, Options) ->
    {ok, ConfigFromFile} = file:consult("etc/mas.config"),
    ConfigWithEnv = [{agent_env,Module}|ConfigFromFile],
    OverwrittenConfig = misc_util:overwrite_options(Options, ConfigWithEnv),
    ConfigRecord = proplist_to_record(OverwrittenConfig),
    Model:start(Time, SP, ConfigRecord),
    ok.


%% @doc Transform a proplist with config properties to a record
-spec proplist_to_record([tuple()]) -> config().
proplist_to_record(Proplist) ->
    Dict = dict:from_list(Proplist),
    #config{?LOAD(agent_env, Dict),
            ?LOAD(topology, Dict),
            ?LOAD(log_dir, Dict),
            ?LOAD(islands, Dict),
            ?LOAD(population_size, Dict),
            ?LOAD(write_interval, Dict),
            ?LOAD(arena_timeout, Dict)}.