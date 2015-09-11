%%% @author John <john@john-K55VD>
%%% @copyright (C) 2013, John
%%% Simple test module for testing the servers
%%% Just remember to change the time in the servers you wish to test
%%% As they are running off localtime, you'll need to hardcode the time you want.
%%% Created : 15 Nov 2013 by John <john@john-K55VD>

-module(test_con).
-export([test_nasdaq/0, test_lse/0, test_dax/0, stop/0, basic_init/0]).

test_nasdaq() ->
    basic_init(),
    inets:start(httpc, [{profile,nasdaq_inets_profile}]),
    nasdaq_server:start_link().
test_lse() ->
    basic_init(),
    timer:sleep(100),
    inets:start(httpc, [{profile,lse_inets_profile}]),
    timer:sleep(100),
    lse_server:start_link().
test_dax() ->
    basic_init(),
    crypto:start(),
    inets:start(httpc, [{profile,dax_inets_profile}]),
    dax_server:start_link().

basic_init() ->
    inets:start(),
    batcher_server:start_link(),
    logger_server:start_link().

stop() ->
    inets:stop(),
    ssl:stop(),
    crypto:stop(),
    batcher_server:stop(),
    logger_server:stop(),
    inets:stop(httpc,[{profle, nasdaq_inets_profile}]),
    inets:stop(httpc,[{profle, lse_inets_profile}]),
    inets:stop(httpc,[{profle, dax_inets_profile}]),
    nasdaq_server:stop(),
    lse_server:stop(),
    dax_server:stop().

