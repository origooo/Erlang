%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-194126.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc nasdaq_server handles fetching data from the nasdaq / market on demand
%%% website. 
%%% @end
%%% Created : 31 Oct 2013 by Patrik Bäckström <origooo@dhcp-194126.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(nasdaq_server).
-include("definitions.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal functions
-export([loop/0,fetch/4]).

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
    inets:start(httpc,[{profile,?NASDAQ_INETS_PROFILE}]),
    process_flag(trap_exit,true),
    register(?NASDAQ_LOOP_NAME,spawn_link(fun() -> loop() end)),
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
    inets:stop(httpc,?NASDAQ_INETS_PROFILE),
    case whereis(?NASDAQ_LOOP_NAME) of
	undefined -> ok;
	_ -> exit(whereis(?NASDAQ_LOOP_NAME),Reason)
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
%% @doc loop/0 is the basic loop that the server waits in. It waits for the correct local
%% time before using the symbols module, applying a funciton to the returned list
%% from that module then sending them off to fetch the data.
%% @spec loop()-> any()
%% @end
%%--------------------------------------------------------------------

loop() ->
    {{_,_,_},{H,M,_S}}=erlang:localtime(),
    %H=16,
    %H=17
    %M=10,
    Fun=fun(Symbol) -> 
		spawn(fun() -> Query=lists:concat([?NASDAQ_SOURCE_URL,Symbol]),
			      fetch(Symbol,0,Query,normal)
		      end)
	end,
    
    case M of
	
	%% First day fetch, adds historical data to the database.
	?NASDAQ_FETCH_MIN when H+?NASDAQ_TIMEZONE_ADJUSTMENT=:=?NASDAQ_OPEN_HOUR ->
	    
	    LastFun = fun(Symbol)->
		spawn(fun() -> Query=lists:concat([?NASDAQ_SOURCE_URL,Symbol]),
			      fetch(Symbol,0,Query,last)
		      end)		      end,
	    {ok,List}=symbols:get_stock({?MODULE,?NASDAQ_INETS_PROFILE,[]}),
	    lists:foreach(LastFun,List),
	    timer:sleep(?MIN),
	    loop();

	%% Standard fetch, adds normal data to the database.
	?NASDAQ_FETCH_MIN when H+?NASDAQ_TIMEZONE_ADJUSTMENT>=?NASDAQ_OPEN_HOUR,
			       H+?NASDAQ_TIMEZONE_ADJUSTMENT=<?NASDAQ_CLOSE_HOUR ->
	    {ok,List}=symbols:get_stock({?MODULE,?NASDAQ_INETS_PROFILE,[]}),
	    lists:foreach(Fun,List),
	    timer:sleep(?MIN),
	    loop();
	_ -> timer:sleep(?MIN),loop()
    end.

%%--------------------------------------------------------------------
%% @doc fetch/4 fetches the data from the source specifically for nasdaq.
%% like the other sources it has a waiting time per proccess
%% @spec fetch(string(), integer, string(), atom()) -> tuple()
%% @end
%%--------------------------------------------------------------------


fetch(Symbol,Try,Query,Type) when Try < ?MAX_RETRIES ->
    timer:sleep(crypto:rand_uniform(5,3000)*100),
    try
	Result=httpc:request(get,
			     {Query,[]},
			     [],
			     [],
			     ?NASDAQ_INETS_PROFILE),
	case Result of
	    {ok,{_,_,X}} ->
		%% Successful request, data is now checked agaisnt the markets
		%% reply. If it contains the SYMBOL_NOT_FOUND string then it's
		%% not a correct reply.
		case string:str(X,?NASDAQ_SYMBOL_NOT_FOUND) of
		    0 ->
			Length=string:len(X),
			Data=string:substr(X,19,Length-19),
			%% Data is sent to the batcher and the logger is notified of a success.
	    	        ?BATCHER_LOOP_NAME!{self(),Symbol,Data,?SERVER,Type},
		        receive
		            {ok,?BATCHER_LOOP_NAME} -> 
				?LOGGER_LOOP_NAME!{success,{timestamp,erlang:localtime()},
					{server,?NASDAQ_LOOP_NAME},
					{symbol,Symbol},
					{data_sent_to_batcher}},
				exit(normal)
		        after 3000 ->
				exit(normal)
		        end;
		    %% Catches symbols that don't return the correct data.
		    _ -> 
			?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
				{server,?NASDAQ_LOOP_NAME},
				{symbol,Symbol},
				{symbol_cannot_be_fetched}},
			exit(normal)
		end;

	    %% Catches errors either from a closed connection or
	    %% a failed connection to a source.
	    {error,socket_closed_remotely} ->
		?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
			{server,?NASDAQ_LOOP_NAME},
			{symbol,Symbol},
			{retry,Try+1},
			{socket_closed_remotely}},
		timer:sleep(crypto:rand_uniform(5,120)*1000),
		fetch(Symbol,Try+1,Query,Type);
	    {error,{failed_connect,_}} -> 
		?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
				   {server,?NASDAQ_LOOP_NAME},
				   {symbol,Symbol},
				   {retry,Try+1},
				   {connection_failed}},
		timer:sleep(crypto:rand_uniform(5,120)*1000),
		fetch(Symbol,Try+1,Query,Type);
	    Other -> io:format("Result: ~p",[Other])
	    end
    catch
	_ -> io:format("Huh? ~n",[]),
	     exit(normal)
    end;


%%--------------------------------------------------------------------
%% @doc fetch/3 this version of fetch is triggered when more than 3 attempts
%% have been made. It sends an error to the logger with the reason.
%% @spec fetch(string(), integer, any(),any()) -> exit(reason)
%% @end
%%--------------------------------------------------------------------
fetch(Symbol,Try,_Query,_Type) ->
    ?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
	    {server,?NASDAQ_LOOP_NAME},
	    {symbol,Symbol},
	    {retries,Try},
	    {timeout}},
    exit(normal).
