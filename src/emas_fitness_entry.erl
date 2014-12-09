%%%-------------------------------------------------------------------
%%% @author jstypka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. gru 2014 17:17
%%%-------------------------------------------------------------------
-module(emas_fitness_entry).
-behaviour(exometer_entry).

%% API
-export([behaviour/0, new/3, delete/3, get_value/4, update/4, reset/3, sample/3, get_datapoints/3, setopts/3]).

-type name()        :: list().
-type type()        :: atom().
-type options()     :: [{atom(), any()}].
-type datapoints()  :: [datapoint()].
-type datapoint()   :: exometer:datapoint().
-type value()       :: any().
-type ref()         :: any().
-type error()       :: {error, any()}.

-define(INIT_FITNESS, -999999).

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    entry.

-spec new(name(), type(), options()) -> ok | {ok, ref()} | error().
new(_Name, _Type, _Options) ->
    Tid = ets:new(fitness_table,[public]),
    ets:insert(Tid, {fitness, ?INIT_FITNESS}),
    {ok, Tid}.

-spec delete(name(), type(), ref()) -> ok | error().
delete(_Name, _Type, Ref) ->
    ets:delete(Ref),
    ok.

-spec get_value(name(), type(), ref(), datapoints()) ->
    [{datapoint(), value()}].
get_value(_Name, _Type, Ref, _Datapoints) ->
    [{fitness, Val}] = ets:lookup(Ref, fitness),
    [{fitness, Val}].

-spec update(name(), value(), type(), ref()) -> ok | {ok, value()} | error().
update(_Name, Value, _Type, Ref) ->
    [{fitness, OldFitness}] = ets:lookup(Ref, fitness),
    ets:insert(Ref, {fitness, max(OldFitness, Value)}),
    ok.

-spec reset(name(), type(), ref()) -> ok | {ok, value()} | error().
reset(_Name, _Type, Ref) ->
    ets:insert(Ref, {fitness, ?INIT_FITNESS}),
    ok.

-spec sample(name(), type(), ref()) -> ok | error().
sample(_, _, _) ->
    ok.

-spec get_datapoints(name(), type(), ref()) -> datapoints().
get_datapoints(_, _, _) ->
    [fitness].

-spec setopts(exometer:entry(), options(), exometer:status()) -> ok | error().
setopts(_, _, _) ->
    ok.
