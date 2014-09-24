-include ("emas.hrl").

-type island() :: [agent()].

-type counter() :: dict:dict().

-type model() :: sequential | hybrid | concurrent | skel.

-type funstat() :: {Name::atom(), Map::fun(), Reduce::fun(), Value::term()}.