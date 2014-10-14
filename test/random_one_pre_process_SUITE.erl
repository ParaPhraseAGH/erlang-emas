-module(random_one_pre_process_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([should_be_called_in_each_worker/1,
         one_process_should_call_random_only_once/1]).


all() ->
    [should_be_called_in_each_worker,
     one_process_should_call_random_only_once].

init_per_testcase(_AnyTestCase, _Conf) ->
    _Conf.


end_per_testcase(_AnyTestCase, _Conf) ->
    meck:unload().



should_be_called_in_each_worker(_Conf) ->
    %% GIVEN
    meck:expect(misc_util, seed_random, 0, ok),
    NumberOfWorkers = 4,

    %% WHEN
    %% four processes with random
    skel:do( [{map, [fun (Any) ->
                             skel_main:seed_random_once_per_process(),
                             Any
                     end],
               NumberOfWorkers}],
             _Data = [[a, b, c, d, e]]),

    %% THEN
    meck:wait(NumberOfWorkers, misc_util, seed_random, '_', 2000),
    ok.


one_process_should_call_random_only_once(_Conf) ->
    %% GIVEN
    DataLen = 10,
    meck:expect(misc_util, seed_random, 0, ok),

    meck:new(helpers, [non_strict]),
    meck:expect(helpers, id, fun(Any) ->
                                     Any
                             end),

    %% WHEN
    %% one process is called DataLen times
    skel:do( [{seq,
               fun (Any) ->
                       skel_main:seed_random_once_per_process(),
                       helpers:id(Any)
               end}],
             _Data = lists:seq(1, DataLen )),


    %% THAN
    %% process was callen DataLen times
    meck:wait(DataLen, helpers, id, 1, 2000 ),
    %% but seed_random was called only once
    meck:wait(1, misc_util, seed_random, 0, 2000),
    %% and __only__ once
    try
        should_timeout = meck:wait(2, misc_util, seed_random, 0, 2000)
    catch
        error:timeout ->
            ok
    end.

    






