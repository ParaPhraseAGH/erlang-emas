%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy.

-module(conc_island).
-export([run/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run(Pid) -> ok
%% @doc Funkcja uruchamiajaca supervisora dla wyspy. Argumentem jest pid
%% tzw. krola, czyli procesu spawnujacego wyspy i czekajacego na wynik (zwykle shell).
%% Funkcja spawnuje areny i agentow, czeka na wiadomosci oraz odsyla
%% koncowy wynik do krola. Na koniec nastepuje zamkniecie aren i sprzatanie.
run(King,N,Instance) ->
  Port = spawn(arenas,startPort,[self(),King]),
  Ring = spawn(arenas,startRing,[self()]),
  Bar = spawn(arenas,startBar,[self()]),
  Arenas = [Ring,Bar,Port],
  King ! {arenas,Arenas}, % wysylamy adresy aren do krola, zeby mogl odeslac je portom
  [spawn_monitor(agent,start,Arenas) || _ <- lists:seq(1,config:populationSize())],
  FDs = io_util:prepareWriting(Instance ++ "\\" ++ integer_to_list(N)),
  receiver(0,-99999,FDs,config:populationSize(),Arenas), % obliczanie wyniku
  Bar ! Ring ! Port ! {finish,self()},
  io_util:closeFiles(FDs),
  allDead = cleaner(Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(int) -> float()
%% @doc Funkcja odbierajaca wiadomosci. Moga to byc meldunki o wyniku
%% od baru lub rozkaz zamkniecia wyspy od krola. Argumentem jest licznik
%% odliczajacy kroki do wypisywania, a zwracany jest koncowy wynik.
receiver(Counter,Best,FDs,Population,Arenas) ->
  receive
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      receiver(Counter,Best,FDs,Population - 1,Arenas);
    {newAgents,AgentList} ->
      [spawn_monitor(agent,start,[A|Arenas]) || A <- AgentList],
      Result = misc_util:result(AgentList),
      NewPopulation = Population + length(AgentList),
      io_util:write(dict:fetch(population,FDs),NewPopulation),
      Step = config:printStep(),
      if Counter == Step ->
        io:format("Fitness: ~p, Population: ~p~n",[Result,NewPopulation]),
        NewCounter = 0;
      Counter /= Step ->
        NewCounter = Counter + 1
      end,
      if Best > Result ->
        receiver(NewCounter,Best,FDs,NewPopulation,Arenas);
      Best =< Result ->
        io_util:write(dict:fetch(fitness,FDs),Result),
        receiver(NewCounter,Result,FDs,NewPopulation,Arenas)
      end;
    close ->
      forcedShutdown
  after config:supervisorTimeout() ->
    io:format("Timeout na wyspie ~p~n",[self()]),
    timeout
  end.

%% @spec cleaner(List1) -> allDead | notAllDead
%% @doc Funkcja upewnia sie, ze wszystkie areny z listy przeslanej
%% w argumencie koncza poprawnie swoje dzialanie.
cleaner([]) ->
  allDead;
cleaner(Arenas) ->
  receive
    {finished,Pid} ->
      true = lists:member(Pid,Arenas), %debug
      cleaner(lists:delete(Pid,Arenas));
    {result,_} ->
      cleaner(Arenas)
  after config:supervisorTimeout() ->
    notAllDead
  end.
