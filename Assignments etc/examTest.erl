-module(examTest).

%% API
-export([start/0,stop/0,calc/1,test/0]).

%% Internal
-export([init/0,loop/1]).
-define(SERVER,server).


%%%-----------------------------
%%% API
%%%-----------------------------
start() ->
    process_flag(trap_exit,true),
    register(?SERVER,spawn(?MODULE, init, [])),
    {ok,whereis(?SERVER)}.

stop() ->
    ?SERVER!{self(),stop},
    receive
	stop -> 
	    exit(whereis(?SERVER),ok),
	    {server_stopped}
    end.

calc({Op,Num}) ->
    ?SERVER!{self(),Op,Num},
    receive
	{Requester,done} -> {done,Requester}
    end.

status() ->
    ?SERVER!{self(),status},
    receive
	{status,Sum} -> Sum
    end.

test() ->
    start(),
    spawn(fun() ->
		  calc({plus,5}),calc({minus,3}) end),
    spawn(fun() ->
		  calc({mul,2}) end),
    timer:sleep(300),
    Sum=status(),
    stop(),
    Sum.

%%%----------------------------
%%% Internal
%%%----------------------------

init() ->
    loop(0).

loop(Sum) ->
    %erlang:display(Sum),
    receive
	{From,minus,Num} -> From!{From,done},loop(Sum-Num);
	{From,plus,Num} -> From!{From,done},loop(Sum+Num);
	{From,mul,Num} -> From!{From,done},loop(Sum*Num);
	{From,status} -> From!{status,Sum},loop(0);
	{From,stop} -> From!stop,loop(Sum);
	_ -> loop(Sum)
    end.
