-module (mas).
-export ([start/1]).

-type model() :: sequential_mixed | sequential_lists | hybrid | concurrent.

-spec start(model()) -> ok.
start(Model) ->
    Model:start(),
    ok.