%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2013 by Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(controlUnit).
-compile([ndaxSource]).
-export([init/0,loop/0]).

init() ->
    inets:start(),
    Pid=spawn(controlUnit,loop,[]),
    register(cU,Pid).

loop() ->
    receive
	{write,Msg} -> io:format([Msg,"\n"]),loop();
	{data,Data} -> io:format([Data,"\n"]),loop();
	{look,Source} -> Source!run,loop();
	{stop,Source} -> Source!stop,loop();
	{test,Source} -> Source!test,loop();
	init -> ndaxSource:init(),loop();
	stop -> exit(normal);
	update -> controlUnit:loop();
	_ -> nothing
    end.
