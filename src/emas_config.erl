-module (emas_config).

-compile(export_all).

%% ====================================================================
%% EMAS PARAMETERS
%% ====================================================================

%% @doc the name of the module with the genetic operators and the genetic callback implementation
-ifdef(nifops).
genetic_ops() -> rastrigin_nif_ops.
-else.

-ifdef(binops).
genetic_ops() -> rastrigin_bin_ops.
-else.

-ifdef(labsops).
genetic_ops() -> labs_ops.
-else.
genetic_ops() -> rastrigin_ops.
-endif.

-endif.

-endif.
%% @doc The size of the problem represented by an agent
problemSize() ->  100.


%% @doc Should we monitor population diversity
monitorDiversity() -> false.
%% @doc Agent initial energy
initialEnergy() -> 10.
%% @doc Amount of energy above which agents reproduce
reproductionThreshold() -> 11.
%% @doc Maximal amount of energy given by a parent to its child during reproduction
reproductionTransfer() -> 5.
%% @doc Maximal amount of energy given by the loser to a winner during the fight
fightTransfer() -> 10.

%% @doc How much of a solution should mutate [0,1]
mutationRate() -> 0.1.
%% @doc How big is the mutation
mutationRange() -> 0.05.
%% @doc The probability to mutate a solution during reproduction
mutationChance() -> 0.75.
%% @doc The probability of migration of an agent with positive energy
migrationProbability() -> 0.0001.
%% @doc The probability to recombine parent solutions during reproduction
recombinationChance() -> 0.3.

%% @doc The size of the fight arena
fightNumber() -> 2.