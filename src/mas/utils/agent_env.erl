-module (agent_env).
-export ([behaviour_info/1]).

-spec behaviour_info(callbacks) -> [{fun(),arity()}].

behaviour_info(callbacks) ->
    [
        {initial_population, 0}, 
        {behaviour_function, 1}, 
        {behaviours, 0},
        {meeting_function, 1}
    ];
behaviour_info(_) ->
    undefined.
