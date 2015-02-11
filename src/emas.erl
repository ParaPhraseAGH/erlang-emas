-module (emas).
-behaviour(mas_agent_env).

-export([initial_agent/1,
         behaviour_function/2,
         behaviours/0,
         meeting_function/2]).

-export([start/2]).

-export_type([agent/0, solution/0, solution/1, sim_params/0]).

-include ("emas.hrl").

-define(LOAD(Prop, Dict), Prop = dict:fetch(Prop, Dict)).
-define(FITNESS_ENTRY, emas_fitness_entry_nif).

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
    SP = emas_config:proplist_to_record(ConfigOptions),
    Env = SP#sim_params.genetic_ops,
    SimParams = SP#sim_params{extra = Env:config()},
    Config = mas_config:proplist_to_record([{agent_env, ?MODULE} |
                                            ConfigOptions]),
    io:format("### SimParams ~w~n", [SimParams]),
    io:format("### ConfigRecord: ~w~n", [Config]),
    initialize_exometer(Config),

    Agents = mas:start(Time, SimParams, Config),

    exometer_report:unsubscribe_all(mas_reporter, [global, fitness]),
    exometer:delete([global, fitness]),

    extract_best(Agents).


-spec initial_agent(sim_params()) -> agent().
initial_agent(SP) ->
    S = emas_genetic:solution(SP),
    {S, emas_genetic:evaluation(S, SP), SP#sim_params.initial_energy}.


%% @doc This function chooses a behaviour for the agent based on its energy.
-spec behaviour_function(agent(), sim_params()) -> agent_behaviour().
behaviour_function({_, _, 0}, _SimParams) ->
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


-spec initialize_exometer(config()) -> ok.
initialize_exometer(Cf) ->
    application:ensure_all_started(exometer),
    mas_reporter:add_reporter(Cf),

    exometer_admin:set_default(['_'],
                               ?FITNESS_ENTRY,
                               [{module, ?FITNESS_ENTRY}]),
    exometer:new([global, fitness], ?FITNESS_ENTRY, []),
    exometer_report:subscribe(mas_reporter,
                              [global, fitness],
                              fitness,
                              Cf#config.write_interval).