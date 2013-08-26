%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0

-module(io_util).
-export([print/1, prepareWriting/1, closeFiles/1, write/2, writeIslands/2, print/2, genPath/4]).

%% ====================================================================
%% API functions
%% ====================================================================

write(FD,Value) ->
  file:write(FD,io_lib:fwrite("~p\n",[Value])).

prepareWriting(Path) ->
  file:make_dir(Path),
  {ok, FitnessFD} = file:open(filename:join([Path,"fitness.txt"]),[append,delayed_write,raw]),
  {ok, PopulationFD} = file:open(filename:join([Path,"population.txt"]),[append,delayed_write,raw]),
  dict:store(fitness,FitnessFD,
    dict:store(population,PopulationFD,
      dict:new())).

closeFiles(FDs) ->
  [file:close(FD) || {_,FD} <- dict:to_list(FDs)].

writeIslands([],[]) -> ok;
writeIslands([FD|FDs],[I|Islands]) ->
  write(dict:fetch(fitness,FD),misc_util:result(I)),
  write(dict:fetch(population,FD),length(I)),
  writeIslands(FDs,Islands).

%% @spec print(float()) -> ok
%% @doc Funkcja wypisujaca fitness na ekran
print(Fitness) ->
  io:format("~nProcess: ~p, Fitness: ~p~n",[self(),Fitness]).

print(Fitness,Groups) ->
  io:format("~nProcess: ~p, Fitness: ~p~n",[self(),Fitness]),
  printMoreStats(Groups).

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

assignName(Files,N) ->
  Name = "instance" ++ integer_to_list(N),
  case lists:member(Name,Files) of
    true -> assignName(Files,N+1);
    false -> Name
  end.

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