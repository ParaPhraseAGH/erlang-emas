%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul zawierajacy funkcje pomocnicze dla innych modulow.

-module(emas_util).
-export([behavior/1,  print/1, clearInbox/0, answer/1, cleaner/1, prepareWriting/1, closeFiles/1, write/2]).

%% ====================================================================
%% API functions
%% ====================================================================

write(FD,Value) ->
  file:write(FD,io_lib:fwrite("~p\n",[Value])).

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

closeFiles(FDs) ->
  [file:close(FD) || {_,FD} <- dict:to_list(FDs)].

%% @spec clearInbox() -> ok
%% @doc Funkcja czyszczaca skrzynke procesu
clearInbox() ->
  receive
    _ -> clearInbox()
  after 0 ->
    ok
  end.

prepareWriting(Path) ->
  file:make_dir(Path),
  {ok, FitnessFD} = file:open(Path ++ "\\fitness.txt",[append,delayed_write,raw]),
  dict:store(fitness, FitnessFD, dict:new()).

%% @spec cleaner(SupervisorPid) -> ok
%% @doc Funkcja uruchamiana pod koniec zycia przez areny, odsylajaca na
%% wszystkie zgloszenia sygnal zabijajacy. Dzieki temu gina wszyscy agenci
%% w systemie. Po zakonczeniu funkcji zakonczony jest proces
cleaner(Supervisor) ->
  receive
    {Pid,_Ref,emigration} ->
      exit(Pid,finished),
      cleaner(Supervisor);
    {Pid,Ref,_} ->
      Pid ! {Ref,0},
      cleaner(Supervisor)
  after config:arenaTimeout() ->
    Supervisor ! {finished,self()},
    exit(normal)
  end.

%% @spec answer(AgentList) -> ok
%% @doc Funkcja wysyla wiadomosci do wszystkich agentow w agent list
%% o ich energii.
answer([]) -> ok;
answer([{Pid,Ref,_,Energy}|Tail]) ->
  Pid ! {Ref,Energy},
  answer(Tail).

%% @spec print(float()) -> ok
%% @doc Funkcja wypisujaca fitness na ekran
print(Fitness) ->
  io:format("~nProcess: ~p, Fitness: ~p~n",[self(),Fitness]).

%% ====================================================================
%% Internal functions
%% ====================================================================