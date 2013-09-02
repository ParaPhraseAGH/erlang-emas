%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczej wyspy.

-module(conc_supervisor).
-export([run/4]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run(Pid) -> ok
%% @doc Funkcja uruchamiajaca supervisora dla wyspy. Argumentem jest pid
%% tzw. krola, czyli procesu spawnujacego wyspy i czekajacego na wynik (zwykle shell).
%% Funkcja spawnuje areny i agentow, czeka na wiadomosci oraz odsyla
%% koncowy wynik do krola. Na koniec nastepuje zamkniecie aren i sprzatanie.
run(King,N,Path,ProblemSize) ->
  process_flag(trap_exit, true),
  Port = spawn(arenas,startPort,[self(),King]),
  Ring = spawn(arenas,startRing,[self()]),
  Bar = spawn(arenas,startBar,[self()]),
  Arenas = [Ring,Bar,Port],
  [spawn_link(agent,start,[ProblemSize|Arenas]) || _ <- lists:seq(1,config:populationSize())],
  IslandPath = filename:join([Path,"isl" ++ integer_to_list(N)]),
  FDs = io_util:prepareWriting(IslandPath),
  timer:send_after(config:writeInterval(),write),
  _Result = receiver(-99999,FDs,config:populationSize(),Arenas), % obliczanie wyniku
  Bar ! Ring ! Port ! {finish,self()},
  %io:format("Island ~p best fitness: ~p~n",[N,_Result]),
  io_util:closeFiles(FDs),
  exit(killAllProcesses).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec receiver(int) -> float()
%% @doc Funkcja odbierajaca wiadomosci. Moga to byc meldunki o wyniku
%% od baru lub rozkaz zamkniecia wyspy od krola. Argumentem jest licznik
%% odliczajacy kroki do wypisywania, a zwracany jest koncowy wynik.
receiver(Best,FDs,Population,Arenas) ->
  receive
    {'EXIT',_FromPid,_Reason} ->
      receiver(Best,FDs,Population - 1,Arenas);
    {newAgents,AgentList} ->
      [spawn_link(agent,start,[A|Arenas]) || A <- AgentList],
      Result = misc_util:result(AgentList),
      NewPopulation = Population + length(AgentList),
      if Best > Result ->
        receiver(Best,FDs,NewPopulation,Arenas);
      Best =< Result ->
        receiver(Result,FDs,NewPopulation,Arenas)
      end;
    {Pid,Ref,emigrant,HisPid} ->
      erlang:unlink(HisPid),
      Pid ! {Ref,ok},
      receiver(Best,FDs,Population - 1,Arenas);
    {Pid,Ref,immigrant,HisPid,HisRef} ->
      erlang:link(HisPid),
      HisPid ! {HisRef,Arenas},
      Pid ! {Ref,ok}, % send confirmation
      receiver(Best,FDs,Population + 1,Arenas);
    write ->
      io_util:write(dict:fetch(fitness,FDs),Best),
      io_util:write(dict:fetch(population,FDs),Population),
      timer:send_after(config:writeInterval(),write),
      receiver(Best,FDs,Population,Arenas);
    close ->
      Best
  after config:supervisorTimeout() ->
    io:format("Timeout na wyspie ~p~n",[self()]),
    exit(timeout)
  end.
