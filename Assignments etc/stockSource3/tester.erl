%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created :  2 Oct 2013 by Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(tester).

%% API
-export([start_link/0,stop/0]).

%% Internal functions
-export([loop/0]).

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
%% start_link/0: Spawns the tester loop and returns {ok,Pid} to its bridge.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Pid=spawn(tester,loop,[]),
    register(tester,Pid),
    {ok,Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop()->
    receive
	test -> io:format("testing");
	stop -> exit(normal)
    end.
