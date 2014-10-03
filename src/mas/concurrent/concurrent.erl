%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This is the main module of concurrent model. It handles starting the system and cleaning after work

-module(concurrent).
-export([start/3]).
-include("mas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), sim_params(), config()) -> ok.
start(Time, SP, Cf = #config{islands = Islands}) ->
%%     io:format("{Model=Concurrent,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    misc_util:clear_inbox(),
    topology:start_link(self(), Islands, Cf#config.topology),
    Supervisors = [conc_supervisor:start(SP, Cf) || _ <- lists:seq(1,Islands)],
    logger:start_link(Supervisors, Cf),
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