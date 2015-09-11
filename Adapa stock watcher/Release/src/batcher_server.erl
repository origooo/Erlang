%%%-------------------------------------------------------------------
%%% @author John <john@john-K55VD>
%%% @copyright (C) 2013, John
%%% @doc This module is the batcher server. It handles incomming
%%% messages from the 3 fetcher servers, handles them by attempting to
%%% send them to the database to be stored.
%%% @end
%%% Created :  4 Nov 2013 by John <john@john-K55VD>
%%%-------------------------------------------------------------------
-module(batcher_server).
-behaviour(gen_server).
-include("definitions.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal exports.
-export([send_batch/4]).

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
    inets:start(httpc,[{profile,?BATCHER_INETS_PROFILE}]),
    process_flag(trap_exit, true),
    register(?BATCHER_LOOP_NAME, spawn_link(fun() -> loop() end)),
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
handle_cast(stop, State) ->
    {stop, normal, State};
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
    inets:stop(httpc,?BATCHER_INETS_PROFILE),
    case whereis(?BATCHER_LOOP_NAME) of
	undefined -> ok;
	_ -> exit(whereis(?BATCHER_LOOP_NAME),Reason)
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
%% @doc
%% loop/0 is the main loop for the server. Sitting in a waiting state
%% loop waits for messages from the servers. Depending on which is received
%% it spawns a new process and sends it to the database.
%% @spec loop() -> any()
%% @end
%%--------------------------------------------------------------------

loop() ->
    
    receive
	{From,Symbol,Data,Server,Type} ->
	    From ! {ok,?BATCHER_LOOP_NAME},
	    case Server of
		nasdaq_server -> spawn(fun() -> send_batch(?DATABASE_NAME_NASDAQ,Symbol,Data,Type) end);
		lse_server    -> spawn(fun() -> send_batch(?DATABASE_NAME_LSE,   Symbol,Data,Type) end);
		dax_server    -> spawn(fun() -> send_batch(?DATABASE_NAME_DAX,   Symbol,Data,Type) end)
	    end,
	    loop();

	{stop, From} ->
	    From ! {ok, stopped};
	Message -> io:format("Unkown Message in batcher: ~p\n", [Message]),
        loop()
        
    end.

%%% Functions.
%%--------------------------------------------------------------------
%% @doc send_batch/4 takes the server, symbol and data from the sending 
%% server and sends it over to the database to be entered.
%% @spec send_batch(atom(), string(), string(), atom()) -> httpc:reply()
%% @end
%%--------------------------------------------------------------------


send_batch(Server, Symbol, Data, normal) ->
    
    CouchQuery = lists:concat([?DATABASE_URL, Server, ?DATABASE_UPDATE_HANDLER, Symbol]),
    httpc:request(put,{CouchQuery,[],"application/json",Data},
		  [{version, "HTTP/1.0"}],[]);
send_batch(Server,Symbol,Data, last) ->
    CouchQuery = lists:concat([?DATABASE_URL, Server, ?DATABASE_STORE_HIST, Symbol]),
    httpc:request(put,{CouchQuery,[],"application/json",Data},[{version, "HTTP/1.0"}],[]).

