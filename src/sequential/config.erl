%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Modul zawierajacy parametry algorytmu.

-module(config).
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Rozmiar wektora pojedynczego osobnika
problemSize() -> 40.
%% @doc Warunek stopu. dokladnosc po ktorej osiagnieciu algorytm sie zatrzymuje
stopPrec() -> 1.
%% @doc Ilosc wysp na ktorych generowana jest populacja
islandsNr() -> 2.
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
mutationRate() -> 0.1.
%% @doc Parametr okreslajacy glebokosc pojedynczej mutacji
mutationRange() -> 0.05.
%% @doc Prawdopodobienstwo wystapienia mutacji u osobnika
mutationChance() -> 0.75.
%% @doc Prawdopodobienstwo skrzyzowania osobnikow
recombinationChance() -> 0.3.
%% @doc Prawdopodobienstwo migracji dowolnego osobnika o niezerowej energii na inna wyspe
migrationProbability() -> 0.01.

%% @doc Ilosc milisekund, ktore czeka supervisor na jakas wiadomosc. Jak czas minie, konczy algorytm.
timeout() -> 1000.
