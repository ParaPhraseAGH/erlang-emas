-module (emas_config).

-compile(export_all).

%% ====================================================================
%% EMAS PARAMETERS
%% ====================================================================

%% @doc Nazwa modulu dostarczajacego operatory genetyczne i implementujacego callback genetic
-ifdef(nif).
genetic_ops() -> rastrigin_nif_ops.
-else.
genetic_ops() -> rastrigin_ops.
-endif.

%% @doc Rozmiar problemu reprezentowanego przez agenta
problemSize() ->  100.


%% @doc Czy monitorowac roznorodnosc populacji
monitorDiversity() -> false.
%% @doc Energia poczatkowa osobnika.
initialEnergy() -> 10.
%% @doc Ilosc energii, powyzej ktorej osobnik sie reprodukuje
reproductionThreshold() -> 11.
%% @doc Maksymalna ilosc energi przekazywana potomkowi podczas reprodukcji
reproductionTransfer() -> 5.
%% @doc Maksymalna ilosc energi przekazywana przeciwnikowi podczas walki
fightTransfer() -> 10.

%% @doc Parametr okreslajacy jak duza czesc agenta powinna ulec mutacji
mutationRate() -> 0.1.
%% @doc Parametr okreslajacy glebokosc pojedynczej mutacji
mutationRange() -> 0.05.
%% @doc Prawdopodobienstwo wystapienia mutacji u osobnika
mutationChance() -> 0.75.
%% @doc Prawdopodobienstwo migracji dowolnego osobnika o niezerowej energii na inna wyspe
migrationProbability() -> 0.0001.
%% @doc Prawdopodobienstwo skrzyzowania osobnikow
recombinationChance() -> 0.3.

%% @doc Liczba agentow, na ktorych czeka ring aby rozpoczac walke (kazdy z kazdym)
fightNumber() -> 2.