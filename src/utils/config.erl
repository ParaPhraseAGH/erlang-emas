%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Modul zawierajacy parametry algorytmu.

-module(config).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Rozmiar wektora pojedynczego osobnika
%problemSize() -> 50.
%% @doc Czas wykonywania algorytmu
%totalTime() -> 5000.
%% @doc Ilosc wysp na ktorych generowana jest populacja
%islandsNr() -> 4.

%% @doc Poczatkowa wielkosc populacji
populationSize() -> 100.
%% @doc Energia poczatkowa osobnika.
initialEnergy() -> 10.
%% @doc Ilosc energii, powyzej ktorej osobnik sie reprodukuje
reproductionThreshold() -> 11.
%% @doc Maksymalna ilosc energi przekazywana potomkowi podczas reprodukcji
reproductionTransfer() -> 5.
%% @doc Maksymalna ilosc energi przekazywana przeciwnikowi podczas walki
fightTransfer() -> 10.

%% @doc Parametr okreslajacy jak duza czesc agenta powinna ulec mutacji
mutationRate() -> 0.2.
%% @doc Parametr okreslajacy glebokosc pojedynczej mutacji
mutationRange() -> 0.05.
%% @doc Prawdopodobienstwo wystapienia mutacji u osobnika
mutationChance() -> 0.75.
%% @doc Prawdopodobienstwo skrzyzowania osobnikow
recombinationChance() -> 0.3.
%% @doc Prawdopodobienstwo migracji dowolnego osobnika o niezerowej energii na inna wyspe
migrationProbability() -> 0.0001.

%% @doc Liczba agentow, na ktorych czeka ring aby rozpoczac walke (kazdy z kazdym)
fightNumber() -> 2.

%% @doc Co ile milisekund wpisywany jest wynik do pliku
writeInterval() -> 1000.
%% @doc Ilosc milisekund, ktore czeka supervisor wyspy na jakas wiadomosc. Jak czas minie zamyka wyspe.
supervisorTimeout() -> 5000.
%% @doc Ilosc milisekund, ktore czeka topology na jakas wiadomosc
topologyTimeout() -> 10000.
%% @doc Ilosc milisekund, ktore czeka arena na jakas wiadomosc
arenaTimeout() -> 3000.
%% @doc Ilosc milisekund, ktore czeka agent na jakas wiadomosc
processTimeout() -> 3000.

