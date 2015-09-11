%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-194086.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created :  1 Oct 2013 by Patrik Bäckström <origooo@dhcp-194086.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(looptimer).
-export([start_link/0]).

-export([init/0,loop/0,terminate/0]).

start_link() ->
    Pid=spawn(looptimer,loop,[]),
    register(lt,Pid),
    {ok,Pid}.

init() ->
    %% case whereis(lt) of
    %% 	undefined -> State=state_new;
    %% 	_ -> exit(whereis(lt),reason),State=state_restarted
    %% end,

    %% case Pid=spawn(looptimer,loop,[]) of
    %% 	{undef,_} -> Spawn={error,{loop_not_spawned}};
    %% 	Pid -> Spawn=loop_spawned
    %% end,

    %% case register(lt,Pid) of
    %% 	true -> Register=alias_is_lt;
    %% 	_ -> Register={error,{no_alias_registered}}
    %% end,
    %% {Spawn,Register,State},
    io:format("testingz"),
    init.
    

loop() ->
    receive
	{msg,Msg} -> io:format(Msg,[]),loop();
	test -> io:format("test"),loop();
	stop -> exit(normal)
    after 1000*60*60 ->
	    exit(normal)
    end.

terminate() ->
    %terminate process Pid
    exit(lt,normal).
