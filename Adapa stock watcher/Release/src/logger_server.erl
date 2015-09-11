%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-202082.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc logger_server writes errors to a log file with reasons for failed fetches,
%%% failed connections and successes.
%%%
%%% @end
%%% Created :  1 Nov 2013 by Patrik Bäckström <origooo@dhcp-202082.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(logger_server).
-include("definitions.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal functions
-export([loop/1,write_log/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER,normal).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    register(logger,spawn_link(fun() -> loop([]) end)),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(normal,State) ->
    {stop,normal,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT',_Pid,_Reason},State) ->
    {noreply,State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason,_State) ->
    case whereis(logger) of
	undefined -> ok;
	_ -> exit(whereis(logger),Reason)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc loop/1 loops the database of writes while waiting for messages.
%% @spec loop(list()) -> list()
%% @end
%%--------------------------------------------------------------------

loop(Db) ->
    case length(Db) of
	100 -> loop(write_log(Db));
	_ -> 
	    receive
		{success,{timestamp,Time},
		 {server,Server},
		 {symbol,Symbol},
		 {data_sent_to_batcher}} -> loop(Db++[{success,{Time,Server,Symbol,data_sent_to_batcher}}]);
		{error,{timestamp,Time},
		 {server,Server},
		 {symbol,Symbol},
		 {symbol_cannot_be_fetched}} -> loop(Db++[{error,{Time,Server,Symbol,symbol_cannot_be_fetched}}]);
		{error,{timestamp,Time},
		 {server,Server},
		 {symbol,Symbol},
		 {retry,Retries},
		 {socket_closed_remotely}} -> loop(Db++[{error,{Time,Server,Symbol,socket_closed_remotely,Retries}}]);
		{error,{timestamp,Time},
		 {server,Server},
		 {symbol,Symbol},
		 {retry,Retries},
		 {connection_failed}} -> loop(Db++[{error,{Time,Server,Symbol,connection_failed,Retries}}]);
		{error,{timestamp,Time},
		 {server,Server},
		 {symbol,Symbol},
		 {retries,Retries},
		 {timeout}} -> loop(Db++[{error,{Time,Server,Symbol,timeout,Retries}}]);
		lookup -> erlang:display(Db),loop(Db);
		{test} -> write_log([asdf]),loop(Db);
		_ -> loop(Db)
	    after 5000 ->
		    loop(write_log(Db))
	    end
    end.

%%--------------------------------------------------------------------
%% @doc write_log/1 writes to the log file
%% @spec write_log(list()) -> list()
%% @end
%%--------------------------------------------------------------------


write_log(Elements) ->
    {ok,Path}=file:get_cwd(),
    {ok,IOPid}=file:open([Path,"/ebin/log/log.txt"],[append]),
    write_log(IOPid,Elements).

write_log(IOPid,[]) -> Synch=file:close(IOPid),[];
write_log(IOPid,[{success,{{{_Y,_Mo,_D},{_H,_M,_S}},_Server,_Symbol,_Message}}|_T]) ->
    %%file:write(IOPid,io_lib:format("SUCCESS! ~4..0B-~2..0B-~2..0B,~2..0B:~2..0B:~2..0B --- MESSAGE: ~p GOT: ~s. SYMBOL: ~s~n", [Y,Mo,D,H,M,S,Server,Message,Symbol])),
    write_log(IOPid,[]);
write_log(IOPid,[{error,{{{Y,Mo,D},{H,M,S}},Server,Symbol,Message}}|T]) ->
    file:write(IOPid,io_lib:format("### ERROR ### ~4..0B-~2..0B-~2..0B,~2..0B:~2..0B:~2..0B --- MESSAGE: ~p GOT: ~s. SYMBOL: ~s~n", [Y,Mo,D,H,M,S,Server,Message,Symbol])),
    write_log(IOPid,T);
write_log(IOPid,[{error,{{{Y,Mo,D},{H,M,S}},Server,Symbol,socket_closed_remotely,Retries}}|T]) ->
    file:write(IOPid,io_lib:format("### ERROR ### ~4..0B-~2..0B-~2..0B,~2..0B:~2..0B:~2..0B --- MESSAGE: ~p GOT: ~s. SYMBOL: ~s. Retries: ~p/~p~n", [Y,Mo,D,H,M,S,Server,socket_closed_remotely,Symbol,Retries,?MAX_RETRIES])),
    write_log(IOPid,T);
write_log(IOPid,[{error,{{{Y,Mo,D},{H,M,S}},Server,Symbol,connection_failed,Retries}}|T]) ->
    file:write(IOPid,io_lib:format("### ERROR ### ~4..0B-~2..0B-~2..0B,~2..0B:~2..0B:~2..0B --- MESSAGE: ~p GOT: ~s. SYMBOL: ~s. Retries: ~p/~p~n", [Y,Mo,D,H,M,S,Server,connection_failed,Symbol,Retries,?MAX_RETRIES])),
    write_log(IOPid,T);
write_log(IOPid,[{error,{{{Y,Mo,D},{H,M,S}},Server,Symbol,timeout,Retries}}|T]) ->
    file:write(IOPid,io_lib:format("### ERROR ### ~4..0B-~2..0B-~2..0B,~2..0B:~2..0B:~2..0B --- MESSAGE: ~p GOT: ~s. SYMBOL: ~s. Retries: ~p/~p~n", [Y,Mo,D,H,M,S,Server,timeout,Symbol,Retries,?MAX_RETRIES])),
    write_log(IOPid,T);
write_log(IOPid,[H|T]) ->
    file:write(IOPid,io_lib:format("~p~n",[H])),
    write_log(IOPid,T).
