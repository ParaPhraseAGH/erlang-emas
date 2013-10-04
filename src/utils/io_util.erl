%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul z funkcjami dotyczacymi operacji wejscia/wyjscia (wypisywanie na ekran, zapis do pliku).

-module(io_util).
-export([printSeq/1, prepareWriting/1, closeFiles/1, write/2, writeIslands/4, printMoreStats/1, genPath/4, sumEnergy/1, printArenas/1]).

-record(counters,{fight = 0 :: non_neg_integer(),
  reproduction = 0 :: non_neg_integer(),
  migration = 0 :: non_neg_integer(),
  death = 0 :: non_neg_integer()}).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type island() :: [agent()].
-type task() :: death | fight | reproduction | migration.
-type groups() :: [{task(),[agent()]}].
-type counters() :: #counters{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec write(file:io_device(),term()) -> ok.
%% @doc Funkcja dokonuje buforowanego zapisu do pliku. W argumencie podany deskryptor i wartosc do zapisania.
write(FD,Value) ->
  file:write(FD,io_lib:fwrite("~p\n",[Value])).

-spec prepareWriting(string()) -> FDs :: dict().
%% @doc Funkcja tworzy folder oraz pliki tekstowe do zapisu i zwraca dict() z deskryptorami.
prepareWriting(Path) ->
  file:make_dir(Path),
  lists:foldl(fun(Atom,Dict) ->
                Filename = atom_to_list(Atom) ++ ".txt",
                {ok,Descriptor} = file:open(filename:join([Path,Filename]),[append,delayed_write,raw]),
                dict:store(Atom,Descriptor,Dict)
              end,dict:new(),
              [fitness,population,migration,fight,reproduction,death]).

-spec closeFiles(dict()) -> any().
%% @doc Funkcja zamykająca pliki podane w argumencie
closeFiles(FDs) ->
  [file:close(FD) || {_,FD} <- dict:to_list(FDs)].

-spec writeIslands([port()],[island()],[counters()],float()) -> ok.
%% @doc Funkcja dokonujaca zapisu do pliku dla modelu sekwencyjnego. W argumencie podawana jest lista wysp oraz
%% lista slownikow z deskryptorow. Kolejnosc na liscie determinuje przyporzadkowanie wyspy do danego slownika,
%% takze nie moze ona ulec zmianie podczas innych operacji na tych listach w algorytmie.
writeIslands([],[],[],_) -> ok;
writeIslands([FD|FDs],[I|Islands],[Counters|Rest],PreviousBest) ->
  Fitness = case misc_util:result(I) of
              islandEmpty -> PreviousBest;
              X -> X
            end,
  write(dict:fetch(fitness,FD),Fitness),
  write(dict:fetch(population,FD),length(I)),
  write(dict:fetch(reproduction,FD),Counters#counters.reproduction),
  write(dict:fetch(fight,FD),Counters#counters.fight),
  write(dict:fetch(death,FD),Counters#counters.death),
  %write(dict:fetch(migration,FD),Counters#counters.migration), todo
  writeIslands(FDs,Islands,Rest,PreviousBest).

-spec printSeq([island()]) -> ok.
%% @doc Funkcja wypisujaca na ekran podstawowe parametry dla modelu sekwencyjnego. W argumencie przesylana jest lista wysp.
printSeq([]) -> ok;
printSeq([Island|T]) ->
  io:format("Island ~p Fitness ~p Population ~p Energy ~p~n",[length(T),misc_util:result(Island),length(Island),sumEnergy(Island)]),
  printSeq(T).

-spec sumEnergy([agent()]) -> integer().
%% @doc Funkcja oblicza sume energii w danej populacji.
sumEnergy(Agents) ->
  lists:foldl(fun({_,_,E},Acc) -> Acc + E end,0,Agents).

-spec printMoreStats(groups()) -> any().
%% @doc Funkcja wypisuje dodatkowe informacje na podstawie przeslanej
%% struktury populacji.
printMoreStats(Groups) ->
  D = lists:flatten([X || {death,X} <- Groups]),
  F = lists:flatten([X || {fight,X} <- Groups]),
  R = lists:flatten([X || {reproduction,X} <- Groups]),
  M = lists:flatten([X || {migration,X} <- Groups]),
  io:format("Dying: ~p    Fighting: ~p    Reproducing: ~p    Leaving: ~p~n",[length(D),length(F),length(R),length(M)]),
  io:format("Population: ~p, Energy: ~p~n",[length(D ++ M ++ F ++ R),sumEnergy(M ++ F ++ R)]).

-spec printArenas([pid()]) -> ok.
printArenas([Ring,Bar,Port]) ->
  io:format("Supervisor: ~p~nRing: ~p~nBar: ~p~nPort: ~p~n~n",[self(),Ring,Bar,Port]).

-spec genPath(string(),pos_integer(),pos_integer(),pos_integer()) -> string().
%% @doc Funkcja generujaca sciezke dostepu wraz z nazwa folderu dla danego uruchomienia algorytmu. Wyznaczana jest sciezka
%% dostepu w zaleznosci od parametrow algorytmu i tworzony jest kolejny folder dla kazdego uruchomienia (instance).
%% !!!W obecnej wersji algorytmu nieuzywana!!!
genPath(AlgType,Problem,TTime,Islands) ->
  catch file:make_dir(AlgType),
  Time = TTime div 1000,
  Param = integer_to_list(Problem) ++ "_" ++ integer_to_list(Time) ++ "_" ++ integer_to_list(Islands),
  Path = filename:join([AlgType, Param]),
  catch file:make_dir(Path),
  {ok,Instances} = file:list_dir(Path),
  Path2 =  filename:join([Path,assignName(Instances,0)]),
  file:make_dir(Path2),
  Path2.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec assignName([string()],integer()) -> string().
%% @doc Funkcja wyznacza nazwe dla nowego folderu na podstawie otrzymanej listy juz istniejacych katalogow.
%% !!!W obecnej wersji algorytmu nieuzywana!!!
assignName(Files,N) ->
  Name = "instance" ++ integer_to_list(N),
  case lists:member(Name,Files) of
    true -> assignName(Files,N+1);
    false -> Name
  end.