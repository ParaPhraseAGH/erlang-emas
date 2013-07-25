%% @author krzywick
%% @doc @todo Add description to config.


-module(config).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

steps() -> 1000.
populationSize() -> 50.
initialEnergy() -> 10.

reproductionTransfer() -> 5.
fightTransfer() -> 10.


%% ====================================================================
%% Internal functions
%% ====================================================================


