-module (mas).
-export ([start/4]).

-include ("mas.hrl").

-spec start(atom(),model(),pos_integer(),[tuple()]) -> ok.
start(_Module,Model,Time,_Options) ->
    %% TODO Module jest poki co ladowany z config.erl, ale powinien byc argumentem wywolania
    Islands = config:islands(),
    Topology = config: topology(),
    Path = config:logDir(),
    %% TODO Mozliwosc nadpisywania tych parametrow opcjami
    Model:start(Time,Islands,Topology,Path),
    ok.