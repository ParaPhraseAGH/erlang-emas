%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0

-module(io_util).
-export([printSeq/1, prepareWriting/1, closeFiles/1, write/2, writeIslands/2, print/2, genPath/4]).

-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type island() :: [agent()].
-type task() :: death | fight | reproduction | migration.
-type groups() :: [{task(),[agent()]}].

%% ====================================================================
%% API functions
%% ====================================================================

-spec write(file:io_device(),term()) -> ok.
write(FD,Value) ->
  file:write(FD,io_lib:fwrite("~p\n",[Value])).

-spec prepareWriting(string()) -> FDs :: dict().
prepareWriting(Path) ->
  file:make_dir(Path),
  {ok, FitnessFD} = file:open(filename:join([Path,"fitness.txt"]),[append,delayed_write,raw]),
  {ok, PopulationFD} = file:open(filename:join([Path,"population.txt"]),[append,delayed_write,raw]),
  dict:store(fitness,FitnessFD,
    dict:store(population,PopulationFD,
      dict:new())).

-spec closeFiles(dict()) -> any().
closeFiles(FDs) ->
  [file:close(FD) || {_,FD} <- dict:to_list(FDs)].

-spec writeIslands([port()],[island()]) -> ok.
writeIslands([],[]) -> ok;
writeIslands([FD|FDs],[I|Islands]) ->
  write(dict:fetch(fitness,FD),misc_util:result(I)),
  write(dict:fetch(population,FD),length(I)),
  writeIslands(FDs,Islands).

-spec printSeq([island()]) -> ok.
printSeq([]) -> ok;
printSeq([Island|T]) ->
  io:format("Island ~p Fitness ~p Population ~p Energy ~p~n",[length(T),misc_util:result(Island),length(Island),sumEnergy(Island)]),
  printSeq(T).

-spec print(float(),groups()) -> any().
print(Fitness,Groups) ->
  io:format("~nProcess: ~p, Fitness: ~p~n",[self(),Fitness]),
  printMoreStats(Groups).

-spec genPath(string(),pos_integer(),pos_integer(),pos_integer()) -> string().
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
assignName(Files,N) ->
  Name = "instance" ++ integer_to_list(N),
  case lists:member(Name,Files) of
    true -> assignName(Files,N+1);
    false -> Name
  end.

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

-spec sumEnergy([agent()]) -> integer().
%% @doc Funkcja oblicza sume energii w danej populacji (liscie agentow).
sumEnergy(Agents) ->
  lists:foldr(fun({_,_,E},Acc) -> Acc + E end,0,Agents).