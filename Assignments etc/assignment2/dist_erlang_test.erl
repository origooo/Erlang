%%% @author Hans Svensson <>
%%% @doc
%%% Test code
%%% @end
%%% Created :  7 Oct 2011 by Hans Svensson <>

-module(dist_erlang_test).
-compile(export_all).
-import(dist_erlang, [fetch/1, store/2, flush/0, start/0]).
test() ->
  start(),
  spawn(fun() ->
            erlang:yield(),
            io:format("Process ~p started!\n", [self()]),
            store(key1, val1), store(key2, val2),
            io:format("~p fetch(key1): ~p\n", [self(), fetch(key1)]),
            io:format("~p fetch(key2): ~p\n", [self(), fetch(key2)]),
            flush(),
            io:format("~p fetch(key1): ~p\n", [self(), fetch(key1)])
        end),
  spawn(fun() ->
            io:format("Process ~p started!\n", [self()]),
            store(key1, val2), store(key2, val1), %% Values swapped!
            io:format("~p fetch(key1): ~p\n", [self(), fetch(key1)]),
            io:format("~p fetch(key2): ~p\n", [self(), fetch(key2)]),
            store(key1, blafooo),
            io:format("~p fetch(key1): ~p\n", [self(), fetch(key1)])
        end),
  ok.
