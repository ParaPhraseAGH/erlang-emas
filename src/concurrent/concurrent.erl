%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul modelu wspolbieznego algorytmu. Odpowiada za uruchomienie calego systemu oraz posprzatanie po wszystkim.

-module(concurrent).
-export([start/0, start/1, start/5]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path) ->
    io:format("{Model=Concurrent,ProblemSize=~p,Time=~p,Islands=~p,Topology=~p}~n",[ProblemSize,Time,Islands,Topology]),
    misc_util:clearInbox(),
    conc_topology:start_link(self(),Islands,Topology),
    Supervisors = [conc_supervisor:start() || _ <- lists:seq(1,Islands)],
    logger:start_link({parallel,Supervisors},Path),
    receive
        ready ->
            trigger(Supervisors,ProblemSize)
    end,
    timer:sleep(Time),
    [conc_supervisor:close(Pid) || Pid <- Supervisors],
    logger:close(),
    conc_topology:close().

-spec start([string()]) -> ok.
start([A,B,C,D,E]) ->
    start(list_to_integer(A),
          list_to_integer(B),
          list_to_integer(C),
          list_to_atom(D),E).

-spec start() -> ok.
start() ->
    file:make_dir("tmp"),
    start(40,5000,2,mesh,"tmp").

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec trigger([pid()],pos_integer()) -> [ok].
trigger(Supervisors,ProblemSize) ->
    [conc_supervisor:go(Pid,ProblemSize) || Pid <- Supervisors].