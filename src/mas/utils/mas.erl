%% @doc This module starts the mas framework with given environment, model and parameters

-module (mas).
-export ([start/4]).

-include ("mas.hrl").

-spec start(atom(),model(),pos_integer(),[tuple()]) -> ok.
start(_Module,Model,Time,_Options) ->
    %% TODO Module jest poki co ladowany z config.erl, ale powinien byc argumentem wywolania
    %% TODO for now the environment module name is hardcoded in config.erl, 
    %% but it should be retrieved from the first parameter (_Module)
    Islands = config:islands(),
    Topology = config: topology(),
    Path = config:logDir(),
    %% TODO Overriding default config parameters with _Options
    Model:start(Time,Islands,Topology,Path),
    ok.