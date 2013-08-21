%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul zawierajacy funkcje pomocnicze dla innych modulow.

-module(emas_util).
-export([rambo/1, result/1, groupBy/2, behavior/1, write/1, clearInbox/0, shuffle/1, checkIfDead/1, optionalPairs/1, print/2, addImmigrants/1]).

%% ====================================================================
%% API functions
%% ====================================================================

write([]) -> ok;
write([{FD,Value}|T]) ->
  file:write(FD,io_lib:fwrite("~p\n",[Value])),
  write(T).

%% @spec groupBy(function(),List1) -> List2
%% @doc Funkcja grupujaca agentow do krotek przy pomocy funkcji F.
%% Zwracana jest lista w formie [{migration,[A1,A2]},{fight,[A3,A4,A5]}]
groupBy(F, L) ->
  dict:to_list(
    lists:foldr(fun({K,V}, D) ->
      dict:append(K, V, D)
    end , dict:new(), [ {F(X), X} || X <- L ])).

%% @spec shuffle(List1) -> List2
%% @doc Funkcja mieszajaca podana liste.
shuffle(L) ->
  Rand = [{random:uniform(), N} || N <- L],
  [X||{_,X} <- lists:sort(Rand)].

%% @spec behavior(Agent) -> death | migration | reproduction | fight
%% @doc Funkcja przyporzadkowujaca agentowi dana klase, na podstawie
%% jego energii.
behavior({_,_,0}) ->
  death;
behavior({_, _, Energy}) ->
  case random:uniform() < config:migrationProbability() of
    true -> migration;
    false -> case Energy > config:reproductionThreshold() of
               true -> reproduction;
               false -> fight
             end
  end.

%% @spec addImmigrants(List1) -> List2
%% @doc Funkcja przegladajaca skrzynke i dorzucajaca czekajacych tam
%% agentow do podanej w argumencie listy.
addImmigrants(Agents) ->
  Pid = whereis(supervisor),
  receive
    {agent,Pid,A} -> addImmigrants([A|Agents])
  after 0 ->
    Agents
  end.

%% @spec rambo(List1) -> ok
%% @doc Funkcja zabijajaca wszystko, co znajdzie na przeslanej liscie.
rambo([])->
  ok;
rambo([H|T]) ->
  H ! {finish,self()},
  rambo(T).

%% @spec checkIfDead(List1) -> ok
%% @doc Funkcja upewniajaca sie, ze wszystkie wyspy zostaly zakonczone
checkIfDead([]) ->
  ok;
checkIfDead(Pids) ->
  receive
    {'DOWN',_Ref,process,Pid,_Reason} ->
      checkIfDead(lists:delete(Pid,Pids))
  after 1000 ->
    io:format("Nie wszystkie wyspy sie zakonczyly~n"),
    timeout
  end.

%% @spec clearInbox() -> ok
%% @doc Funkcja czyszczaca skrzynke i wypisujaca na wyjscie, co tam znalazla
clearInbox() ->
  clearInbox(0,0).

%% @spec optionalPairs(List1) -> List2
%% @doc Funkcja dzielaca podana liste agentow na pary. Przykladowe dzialanie:
%% [A1,A2,A3,A4,A5] -> [{A1,A2},{A3,A4},{A5}]
optionalPairs(L) ->
  optionalPairsTail(L,[]).

%% @spec print(float(),List1) -> ok
%% @doc Funkcja wypisujaca podstawowe informacje na ekran w danym kroku
print(Fitness,Groups) ->
  io:format("~nProcess: ~p, Fitness: ~p~n",[self(),Fitness]),
  printMoreStats(Groups).

%% @spec result(List1) -> float() | islandEmpty
%% @doc Funkcja okreslajaca najlepszy wynik na podstawie przeslanej listy agentow
result(Agents) ->
  case Agents of
    [] ->
      islandEmpty;
    _ ->
      lists:max([ Fitness || {_ ,Fitness, _} <- Agents])
  end.
%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec printMoreStats(List1) -> ok
%% @doc Funkcja wypisuje dodatkowe informacje na podstawie przeslanej
%% struktury populacji.
printMoreStats(Groups) ->
  D = lists:flatten([X || {death,X} <- Groups]),
  F = lists:flatten([X || {fight,X} <- Groups]),
  R = lists:flatten([X || {reproduction,X} <- Groups]),
  M = lists:flatten([X || {migration,X} <- Groups]),
  io:format("Dying: ~p    Fighting: ~p    Reproducing: ~p    Leaving: ~p~n",[length(D),length(F),length(R),length(M)]),
  io:format("Population: ~p, Energy: ~p~n",[length(D ++ M ++ F ++ R),sumEnergy(M ++ F ++ R)]).

%% @spec sumEnergy(List1) -> int()
%% @doc Funkcja oblicza sume energii w danej populacji (liscie agentow).
sumEnergy(Agents) ->
  lists:foldr(fun({_,_,E},Acc) -> Acc + E end,0,Agents).

%% @spec optionalPairsTail(List1,List2) -> List3
%% @doc Funkcja dzielaca podana liste agentow na pary. Tail recursion.
optionalPairsTail([],Acc) -> Acc;
optionalPairsTail([A],Acc) -> [{A}|Acc];
optionalPairsTail([A,B|L],Acc) -> optionalPairsTail(L,[{A,B}|Acc]).

%% @spec clearInbox(int(),int()) -> ok
%% @doc Funkcja przegladajaca skrzynke odbiorcza i wypisujaca znaleziska.
%% Agents to liczba znalezionych wiadomosci z agentami, a Results z wynikami.
clearInbox(Agents,Results) ->
  receive
    {agent,_,_} -> clearInbox(Agents+1,Results);
    {result,_} -> clearInbox(Agents,Results+1)
  after 0 ->
    io:format("W inbox suprevisora bylo jeszcze ~p agent msg and ~p result msg~n",[Agents,Results])
  end.