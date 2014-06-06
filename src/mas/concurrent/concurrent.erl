%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Glowny modul modelu wspolbieznego algorytmu. Odpowiada za uruchomienie calego systemu oraz posprzatanie po wszystkim.

-module(concurrent).
-export([start/4]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time,Islands,Topology,Path) ->
%%     io:format("{Model=Concurrent,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    misc_util:clearInbox(),
    topology:start_link(self(),Islands,Topology),
    Supervisors = [conc_supervisor:start() || _ <- lists:seq(1,Islands)],
    logger:start_link(Supervisors, Path),
    receive
        ready ->
            trigger(Supervisors)
    end,
    timer:sleep(Time),
    [ok = conc_supervisor:close(Pid) || Pid <- Supervisors],
    topology:close(),
    logger:close().

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec trigger([pid()]) -> [ok].
trigger(Supervisors) ->
    [conc_supervisor:go(Pid) || Pid <- Supervisors].