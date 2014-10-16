-include_lib("mas/include/mas.hrl").

-type solution() :: [float()].

%% -type model() :: any().
%% -type funstat() :: any().

-record(sim_params, {genetic_ops :: atom(),
                     problem_size :: pos_integer(),
                     monitor_diversity :: boolean(),
                     initial_energy :: integer(),
                     reproduction_threshold :: integer(),
                     reproduction_transfer :: integer(),
                     fight_transfer :: integer(),
                     mutation_rate :: float(),
                     mutation_range :: float(),
                     mutation_chance :: float(),
                     recombination_chance :: float(),
                     fight_number :: pos_integer()}).