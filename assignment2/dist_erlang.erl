%%% Name            : Patrik Bäckström
%%% Personal number : 860623-3370
%%% E-mail          : gusbacpa@student.gu.se

-module(dist_erlang).

%% DO NOT CHANGE THE EXPORT STATEMENT!
-export([start/0, init/0, stop/0, store/2, fetch/1, flush/0,
         task/1, dist_task/1, pmap/2, faulty_task/1
        ]).
%% End of no-change area

-define(SERVER,sts).

%%--------------------------------------------------------------------
%% Problem 1
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% start/0 - Registers a new process named sts if alias is not already used. Assuming that sts is not already used in another context in the virtual machine.
%% @end
%%--------------------------------------------------------------------

start() ->
    case whereis(?SERVER) of
	undefined -> register(sts,spawn(?MODULE,init,[])),{ok,whereis(sts)};
	Pid when is_pid(Pid) -> {ok,Pid}
    end.

%%--------------------------------------------------------------------
%% @doc
%% stop/0 - Stops the server if it exists.
%% @end
%%--------------------------------------------------------------------

stop() ->
    case whereis(?SERVER) of
	undefined -> already_stopped;
	Pid when is_pid(Pid) -> ?SERVER!stopped
    end.

%%--------------------------------------------------------------------
%% @doc
%% store/2 - 
%% @end
%%--------------------------------------------------------------------

store(Key,Value) ->
    ?SERVER!{store,{self(),Key,Value}},
    receive 
    	{ok,no_value} -> {ok,no_value};
	{ok,OldValue} -> {ok,OldValue}
    end.

fetch(Key) ->
    ?SERVER!{fetch,{self(),Key}},
    receive
	{error,not_found} -> {error,not_found};
	{ok,Value} -> {ok,Value}
    end.

flush() ->
    ?SERVER!{flush,self()},
    receive
	{ok,flushed} -> {ok,flushed}
    end.

init() ->
    loop([]).

loop(Database) ->
    receive 
	{store,{Pid,Key,Value}} when is_pid(Pid) ->
	    case exists({Pid,Key,Value},Database,store) of
		{not_existing} ->
		    Pid!{ok,no_value},
		    loop(lists:append([{Pid,Key,Value}],Database));
		{changed,OldValue,DatabaseEdited} ->
		    Pid!{ok,OldValue},
		    loop(lists:append([{Pid,Key,Value}],DatabaseEdited))
		end;
	{fetch,{Pid,Key}} ->
	    case exists({Pid,Key,0},Database,fetch) of
		{error,not_found} ->
		    Pid!{error,not_found},
		    loop(Database);
		{ok,Value} ->
		    Pid!{ok,Value},
		    loop(Database)
	    end;
	{flush,Pid} ->
	    Flushed=delete(Pid,Database,[]),
	    Pid!{ok,flushed},
	    loop(Flushed);
	stopped -> i_am_useless;
	_ -> loop(Database)
    end.


%%--------------------------------------------------------------------
%% Internal functions, problem 1
%%--------------------------------------------------------------------

exists({Pid,Key,_Value},Database,Task) ->
    PidExists=find(Pid,1,Database),
    case PidExists of
	[] when Task==store -> {not_existing};
	[] when Task==fetch -> {error,not_found};
	_ ->
	    KeyExists=find(Key,2,PidExists),
	    case KeyExists of
		[] when Task==store -> {not_existing};
		[] when Task==fetch -> {error,not_found};
		_ when Task==store ->
		    [{_,_,OldValue}]=KeyExists,
		    {changed,OldValue,lists:delete({Pid,Key,OldValue},Database)};
		_ when Task==fetch ->
		    [{_,_,Value}]=KeyExists,
		    {ok,Value}
	    end
    end.

find(_,_,[]) -> [];
find(What,Index,[H|T]) ->
    case lists:keyfind(What,Index,[H]) of
	false -> find(What,Index,T);
	_ -> lists:append([lists:keyfind(What,Index,[H])],find(What,Index,T))
    end.
    
delete(_Pid,[],Acc) -> Acc;
delete(Pid,[{P,K,V}|T],Acc) ->
    case P==Pid of
	true -> delete(Pid,T,Acc);
	false -> delete(Pid,T,[{P,K,V}|Acc])
    end.


%%--------------------------------------------------------------------
%% Problem 2
%%--------------------------------------------------------------------

%% Do not change the following two functions!
task(N) when N < 0; N > 100 -> 
    exit(parameter_out_of_range);
task(N) ->
    timer:sleep(N * 2),
    256 + 17 *((N rem 13) + 3). 

faulty_task(N) when N < 0; N > 100 -> 
    exit(parameter_out_of_range);
faulty_task(N) ->
    timer:sleep(N * 2),
    {_,_,X} = now(),
    case X < 100 of
	false ->
	    256 + 17 *((N rem 13) + 3);
	true  ->
	    throw(unexpected_error)
    end.
%% End of no-change area

dist_task(Data) ->
    ParentPid=self(),
    [receive {X,Result} ->  Result end || X <- 
	    [spawn(fun() -> ParentPid!{self(),(catch task(X))}  end) || X <- Data]].

pmap(Fun,Data) ->
    ParentPid=self(),
    [receive {ChildPid,Result} -> Result end || ChildPid <- 
	    [spawn(fun() -> ParentPid!{self(),(catch Fun(Value))} end) || Value <- Data]].

