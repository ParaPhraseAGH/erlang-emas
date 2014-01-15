%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Modul zawierajacy czesci wspolne dla roznych wersji algorytmu sekwencyjnego.

-module(sequential).
-export([start/6, start/1, start/2, init/4]).

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
    io:format("{Model=Sequential,ProblemSize=~p,Time=~p,Islands=~p,Topology=~p}~n",[ProblemSize,Time,Islands,Topology]),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    {_Time,_Result} = timer:tc(Fun, [ProblemSize,Time,Islands,Topology,Path]),
    topology:close(),
    logger:close(),
    io:format("Total time:   ~p s~nFitness:     ~p~n",[_Time/1000000,_Result]).

%% @doc Funkcja dokonujaca podstawowych przygotowan i zwracajaca liste slownikow deskryptorow.
-spec init(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
init(Time,IslandsNr,Topology,Path) ->
    timer:send_after(Time,theEnd),
    timer:send_after(config:writeInterval(),write),
    topology:start_link(IslandsNr,Topology),
    logger:start_link({sequential,IslandsNr},Path),
    ok.