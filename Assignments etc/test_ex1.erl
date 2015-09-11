%%% File    : test_ex1.erl
%%% Author  :  <Hans@HANS-LAPTOP>
%%% Description : Hello World
%%% Created :  8 Sep 2009 by  <Hans@HANS-LAPTOP>

-module(test_ex1).

-export([hello/0]).

%% Hello world -- in Erlang.
hello() ->
    io:format("Hello world!\n").
