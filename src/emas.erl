-module (emas).
-behaviour(mas_agent_env).

-export([initial_agent/1,
         behaviour_function/2,
         behaviours/0,
         meeting_function/2,
         stats/0]).

-export([start/2
        ]).

-export_type([agent/0, solution/0, solution/1, sim_params/0]).

-include ("emas.hrl").

-define(LOAD(Prop, Dict), Prop = dict:fetch(Prop, Dict)).

-type solution(Any) :: Any.
-type solution() :: solution(any()).
-type agent() :: mas:agent({Solution::solution(),
                            Fitness::float(),
                            Energy::pos_integer()}).

-type sim_params() :: mas:sim_params(#sim_params{}).
-type agent_behaviour() :: mas:agent_behaviour(death | reproduction | fight).

%% ====================================================================
%% Callbacks
%% ====================================================================

-spec start(pos_integer(), [tuple()]) -> agent().
start(Time, ConfigOptions) ->
    SimParams = emas_config:proplist_to_record(ConfigOptions),
    io:format("### SimParams ~w~n", [SimParams]),
    Agents = mas:start(Time,
                       SimParams,
                       [{agent_env, ?MODULE} |
                        ConfigOptions]),
    extract_best(Agents).


-spec initial_agent(sim_params()) -> agent().
initial_agent(SP) ->
    S = emas_genetic:solution(SP),
    {S, emas_genetic:evaluation(S, SP), SP#sim_params.initial_energy}.


%% @doc This function chooses a behaviour for the agent based on its energy.
-spec behaviour_function(agent(), sim_params()) -> agent_behaviour().
behaviour_function({_,_,0}, _SimParams) ->
    death;

behaviour_function({_, _, Energy}, #sim_params{reproduction_threshold = RT}) ->
    case Energy > RT of
        true -> reproduction;
        false -> fight
    end.


-spec behaviours() -> [agent_behaviour()].
behaviours() ->
    [reproduction, death, fight].


-spec meeting_function({agent_behaviour(), [agent()]}, sim_params()) ->
                              [agent()].
meeting_function({death, _}, _SP) ->
    [];

meeting_function({reproduction, Agents}, SP) ->
    lists:flatmap(fun(Pair) ->
                          emas_evolution:do_reproduce(Pair, SP)
                  end, emas_evolution:optional_pairs(Agents, []));

meeting_function({fight, Agents}, SP) ->
    lists:flatmap(fun(Pair) ->
                          emas_evolution:do_fight(Pair, SP)
                  end, emas_evolution:optional_pairs(Agents, []));

meeting_function({_, _}, _SP) ->
    erlang:error(unexpected_behaviour).


-spec stats() -> [funstat()].
stats() ->
    Fitness_map = fun({_Solution, Fitness, _Energy}) ->
                          Fitness
                  end,
    Fitness_reduce = fun(F1, F2) ->
                             lists:max([F1, F2])
                     end,
    [{fitness, Fitness_map, Fitness_reduce, -999999}].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec extract_best([agent()]) -> agent().
extract_best(Agents) ->
    ArgMax = fun (A = {_, F, _}, {_, AccF, _}) when F > AccF ->
                     A;
                 (_, Acc) ->
                     Acc
             end,
    {_Sol, _Fit, _Energy} = lists:foldl(ArgMax, hd(Agents), tl(Agents)).
