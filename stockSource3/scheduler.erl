%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created :  2 Oct 2013 by Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(scheduler).

%% API
-export([start_link/0,stop/0]).

%% Internal functions
-export([loop/0]).

-define(DELAY10,1000*60*10).
-define(DELAY13,1000*60*13).
-define(DELAY2,1000*60*2).
-define(SOURCES,[ndax,source1,source2,source3,tester]).

%%%===================================================================
%%% API
%%%===================================================================
stop() ->
    case whereis(?MODULE) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% start_link/0: Spawns the scheduler loop and returns {ok,Pid} to its bridge.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Pid=spawn(scheduler,loop,[]),
    register(scheduler,Pid),
    inets:start(),
    {ok,Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop() ->
    receive
	test -> 
	    io:format("testing"),loop();
	stop -> 
	    exit(normal);
	{run,Source} -> 
	    spawn(Source,lookup,[]),loop();
	{schedule,Source,hourly,TimeDiff} -> 
	    spawn(job,job,{hourly,{Source,10,16},TimeDiff}),loop();
	{schedule,Source,daily,Hour} -> 
	    spawn(job,job,{daily,{Source,Hour}}),loop();
	{schedule,all,hourly} -> 
	    io:format("not implemented yet..."),loop();
	_ -> 
	    io:format("unknown message..."),loop()
    end.
