%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This is the main module of concurrent model. It handles starting the system and cleaning after work

-module(mas_concurrent).
-export([start/3, send_result/1]).
-include("mas.hrl").

-define(RESULT_SINK, result_sink).
-define(RESULT_SINK_TIMEOUT, 2500).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), sim_params(), config()) -> ok.
start(Time, SP, Cf = #config{islands = Islands}) ->
%%     io:format("{Model=Concurrent,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    mas_misc_util:clear_inbox(),
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    Supervisors = [mas_conc_supervisor:start(SP, Cf) || _ <- lists:seq(1,Islands)],
    mas_logger:start_link(Supervisors, Cf),
    receive
        ready ->
            trigger(Supervisors)
    end,
    register(?RESULT_SINK, self()),
    timer:sleep(Time),
    [ok = mas_conc_supervisor:close(Pid) || Pid <- Supervisors],
    mas_topology:close(),
    mas_logger:close(),
    Agents = receive_results(),
    unregister(?RESULT_SINK),
    Agents.

-spec send_result(mas:agent()) -> ok.
send_result(Agent) ->
    whereis(?RESULT_SINK) ! {result, Agent}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec trigger([pid()]) -> [ok].
trigger(Supervisors) ->
    [mas_conc_supervisor:go(Pid) || Pid <- Supervisors].

receive_results() ->
    receive_results([]).

receive_results(Acc) ->
    receive
        {result, Agent} ->
            receive_results([Agent | Acc])
    after ?RESULT_SINK_TIMEOUT ->
            Acc
    end.
