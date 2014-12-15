%%%-------------------------------------------------------------------
%%% @author jstypka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. gru 2014 11:48
%%%-------------------------------------------------------------------
-module(emas_fitness_entry_nif).
-behaviour(exometer_entry).
-on_load(init/0).

%% API
-export([behaviour/0, new/3, delete/3, get_value/4, update/4, reset/3, sample/3,
         get_datapoints/3, setopts/3]).

-type name()        :: list().
-type type()        :: atom().
-type options()     :: [{atom(), any()}].
-type datapoints()  :: [datapoint()].
-type datapoint()   :: exometer:datapoint().
-type value()       :: any().
-type ref()         :: any().
-type error()       :: {error, any()}.

-define (LIBNAME, "fitness_entry_nif").

-spec init() -> ok.
init() ->
    ProjectPath = filename:dirname(filename:dirname(code:which(?MODULE))),
    SOName = filename:join([ProjectPath, priv, ?LIBNAME]),
    ok = erlang:load_nif(SOName, 0).

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    entry.

-spec new(name(), type(), options()) -> ok | {ok, ref()} | error().
new(_Name, _Type, _Options) ->
    {ok, none}.

-spec delete(name(), type(), ref()) -> ok.
delete(_Name, _Type, _Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec get_value(name(), type(), ref(), datapoints()) ->
                       [{datapoint(), value()}].
get_value(_Name, _Type, _Ref, _Datapoints) ->
    erlang:nif_error(nif_not_loaded).

-spec update(name(), value(), type(), ref()) -> ok | {ok, value()}.
update(_Name, _Value, _Type, _Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec reset(name(), type(), ref()) -> ok | {ok, value()}.
reset(_Name, _Type, _Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec sample(name(), type(), ref()) -> ok.
sample(_, _, _) ->
    ok.

-spec get_datapoints(name(), type(), ref()) -> datapoints().
get_datapoints(_, _, _) ->
    [fitness].

-spec setopts(exometer:entry(), options(), exometer:status()) -> ok.
setopts(_, _, _) ->
    ok.
