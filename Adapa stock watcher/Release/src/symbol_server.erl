%%%-------------------------------------------------------------------
%%% @author  <John@JOHN-PC>
%%% @copyright (C) 2013, 
%%% @doc the symbol server resets the symbols daily by dropping their
%%% current databases where they exist
%%% @end
%%% Created : 12 Nov 2013 by  <John@JOHN-PC>
%%%-------------------------------------------------------------------
-module(symbol_server).
-include("definitions.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([update_symbols/0, loop/0]).

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

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER,stop).

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
    inets:start(),
    inets:start(httpc,[{profile,?SYMBOL_SERVER_INETS_PROFILE}]),
    process_flag(trap_exit, true),
    register(?SYMBOL_SERVER_LOOP_NAME, spawn_link(fun() -> loop() end)),
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
    Reply = responsive_test,
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
handle_cast(stop, State) ->
    {stop, shutdown, State};
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
terminate(Reason, _State) ->
    inets:stop(httpc,?SYMBOL_SERVER_INETS_PROFILE),
    case whereis(?SYMBOL_SERVER_LOOP_NAME) of
	undefined -> ok;
	_ -> exit(whereis(?SYMBOL_SERVER_LOOP_NAME),Reason)
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
%% @doc loop/0 waits for the correct time before starting to drop the
%% databases and then re creates them
%% @spec loop() -> any()
%% @end
%%--------------------------------------------------------------------

loop() ->

    {{_,_,_},Time}=erlang:localtime(),
    %H={23,50,00},

    case Time of
	
    	%11:50 pm, can change later if need be.
     	{23,50,0} ->  
     	    update_symbols(),
     	    timer:sleep(900000),
     	    loop();
     	{_,_,_} -> 
     	    timer:sleep(900000),
     	    loop()

    end.
    
%%--------------------------------------------------------------------
%% @doc update_symbols/0 re-updates the symbols and adds them back to the
%% database.
%% @spec update_symbols() -> tuple()
%% @end
%%--------------------------------------------------------------------

update_symbols() ->
    
    db_checker:checker(),
    Pid = self(),
    checker!{drop, symbols, Pid},
    get_reply(),
    checker!{check, symbols, Pid},
    get_reply(),
    checker!{check, views, Pid},
    get_reply(),
    checker!{finished, Pid},
    receive
	
	{ok, Num, completed} ->
	    io:format("Symbols updated, ~p of 6\n completed correctly", 
		      [Num])
    end.

%%--------------------------------------------------------------------
%% @doc get_reply/0 collects replies from checker.
%% @spec get_reply() -> tuple()
%% @end
%%--------------------------------------------------------------------


get_reply() ->
    receive
	Message -> {ok, Message}
    end.
