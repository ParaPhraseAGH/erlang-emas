%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul modelu wspolbieznego algorytmu. Odpowiada za uruchomienie calego systemu oraz posprzatanie po wszystkim.

-module(concurrent).
-export([start/0, start/1, start/5, getAddresses/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(ProblemSize::pos_integer(), Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(ProblemSize,Time,Islands,Topology,Path) ->
    io:format("{Model=Concurrent,ProblemSize=~p,Time=~p,Islands=~p,Topology=~p}~n",[ProblemSize,Time,Islands,Topology]),
    misc_util:clearInbox(),
    topology:start_link(Islands,Topology),
    Supervisors = [conc_supervisor:start(self(),ProblemSize) || _ <- lists:seq(1,Islands)],
    logger:start_link({parallel,Supervisors},Path),
    giveAddresses(Supervisors,Islands),
    timer:sleep(Time),
    [conc_supervisor:close(Pid) || Pid <- Supervisors],
    logger:close(),
    topology:close().

-spec start(list()) -> ok.
start([A,B,C,D,E]) ->
    start(list_to_integer(A),
          list_to_integer(B),
          list_to_integer(C),
          list_to_atom(D),E).

-spec start() -> ok.
start() ->
    file:make_dir("tmp"),
    start(40,5000,2,mesh,"tmp").

%% @doc Funkcja wysylajaca na podany w argumencie adres zapytanie
%% o liste pid supervisorow. Zapytanie synchroniczne.
-spec getAddresses(pid()) -> [pid()].
getAddresses(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(),Ref,getAdresses},
    receive
        {Ref,AllSupervisors} ->
            erlang:demonitor(Ref, [flush]),
            AllSupervisors;
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("The king is dead, long live the king!~n",[]),
            erlang:error(Reason)
    after 5000 ->
            io:format("Proces ~p nie dostal wiadomosci z adresami~n",[self()]),
            erlang:error(timeout)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Funkcja wysyla wszystkim portom pelna liste supervisorow.
%% Po poinformowaniu wszystkich portow (liczba podana w arg), funkcja zwraca ok.
-spec giveAddresses([pid()],integer()) -> ok.
giveAddresses(_,0) -> ok;
giveAddresses(Supervisors,NoIslands) ->
    receive
        {Pid,Ref,getAdresses} ->
            Pid ! {Ref,Supervisors},
            giveAddresses(Supervisors,NoIslands - 1)
    after config:supervisorTimeout() ->
            erlang:error(noMsgFromPorts),
            timeout
    end.