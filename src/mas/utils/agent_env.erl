-module (agent_env).
-export ([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {initial_population, 0}, 
        {behaviour_function, 1}, 
        {behaviours, 0},
        {meeting_functions, 0}
    ];
behaviour_info(_) ->
    undefined.
