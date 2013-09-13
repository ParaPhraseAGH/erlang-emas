%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul modelu sekwencyjnego zawierajacy cala logike tej wersji aplikacji.

-module(sequential).
-export([start/6, start/1, start/2, init/6]).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(fun()) -> ok.
start(Fun) ->
  file:make_dir("tmp"),
  Fun(40,5000,2,mesh,"tmp").

-spec start(list(),fun()) -> ok.
start([A,B,C,D,E],Fun) ->
  Fun(list_to_integer(A),
    list_to_integer(B),
    list_to_integer(C),
    list_to_atom(D),E).

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string(), fun()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path,Fun) ->
  random:seed(erlang:now()),
  misc_util:clearInbox(),
  {_Time,{_Result,FDs}} = timer:tc(Fun, [ProblemSize,Time,Islands,Topology,Path]),
  [io_util:closeFiles(FDDict) || FDDict <- FDs],
  topology:close(),
  io:format("Total time:   ~p s~nFitness:     ~p~n",[_Time/1000000,_Result]).

-spec init(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string(), Population::[term()],fun()) -> {float(),[dict()]}.
%% @doc Funkcja dokonujaca podstawowych przygotowan i przechodzaca do glownej petli.
%% Zwracany jest koncowy wynik.
init(Time,IslandsNr,Topology,Path,Population,Loop) ->
  FDs = [io_util:prepareWriting(filename:join([Path,"isl" ++ integer_to_list(N)])) || N <- lists:seq(1,IslandsNr)],
  timer:send_after(Time,theEnd),
  timer:send_after(config:writeInterval(),write),
  topology:start_link(IslandsNr,Topology),
  Loop(Population,FDs).