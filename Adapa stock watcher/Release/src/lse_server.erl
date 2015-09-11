%%% @author John <john@john-K55VD>
%%% @copyright (C) 2013, John
%%% @doc lse server handles fetching from the london stock exchanged
%%% via google apps script.
%%% @end
%%% Created :  5 Nov 2013 by John <john@john-K55VD>

-module(lse_server).
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
    inets:start(httpc,[{profile,?LSE_INETS_PROFILE}]),
    process_flag(trap_exit,true),
    register(?LSE_LOOP_NAME,spawn_link(fun() -> loop() end)),
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
    inets:stop(httpc,?LSE_INETS_PROFILE),
    case whereis(?LSE_LOOP_NAME) of
	undefined -> ok;
	_ -> exit(whereis(?LSE_LOOP_NAME),Reason)
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
%% @doc loop/0 like the other loops waits for messages or the correct
%% local time to occur, it then collects symbols and spawns a procces per
%% each symbol. They are then collected and sent to the batcher.
%% @spec loop() -> any()
%% @end
%%--------------------------------------------------------------------

loop() ->

    {{_,_,_},{H,M,_S}}=erlang:localtime(),
    %M=30,
    %H=10,
    Fun=fun(Symbol) -> 
		spawn(fun() -> Query=lists:concat([?LSE_SOURCE_URL2,Symbol]),
			      fetch(Symbol,0,Query,normal)
		      end)
	end,

	io:format("~p, ~p\n", [H+?LSE_TIMEZONE_ADJUSTMENT, ?LSE_OPEN_HOUR]),
    case M of

	%% First day fetch, adds historical data to the database.
	?LSE_FETCH_MIN when H+?LSE_TIMEZONE_ADJUSTMENT=:=?LSE_OPEN_HOUR ->
	    LastFun=fun(Symbol) -> spawn(fun()->Query=lists:concat([?LSE_SOURCE_URL2,Symbol]),
						fetch(Symbol,0,Query,last)end ) end,
	    {ok,List}=symbols:get_stock({?MODULE,?LSE_INETS_PROFILE,[]}),
	    lists:foreach(LastFun,List),
	    timer:sleep(?MIN),
	    loop();

	%% Standard fetch, adds normal data to the database.
	?LSE_FETCH_MIN when H+?LSE_TIMEZONE_ADJUSTMENT>=?LSE_OPEN_HOUR,
			       H+?LSE_TIMEZONE_ADJUSTMENT=<?LSE_CLOSE_HOUR ->

	    {ok,List}=symbols:get_stock({?MODULE,?LSE_INETS_PROFILE,[]}),
	    lists:foreach(Fun,List),
	    timer:sleep(?MIN),
	    loop();
	_ ->timer:sleep(?MIN),loop()
    end.

%%--------------------------------------------------------------------
%% @doc fetch/4 like the other variants collects the symbol data and
%% sends it to the batcher. Max retries is the limit to how many
%% re-attempts are allowed to be made. As per the others there is a random
%% sleep time for each proccess.
%% @spec fetch(string(), integer(), string(),atom()) -> tuple()
%% @end
%%--------------------------------------------------------------------


fetch(Symbol,Try,Query,Type) when Try < ?MAX_RETRIES ->
    timer:sleep(crypto:rand_uniform(5,6000)*100),

    try
	Reply = httpc:request(get,{Query,[]},[],[],?LSE_INETS_PROFILE),
	case Reply of 
	    {ok, {_Version, _Headers, Data}} ->
		%% Successful request, data is now checked agaisnt the markets
		%% reply. If it contains the SYMBOL_NOT_FOUND string then it's
		%% not a correct reply.

		case string:str(Data,?LSE_SYMBOL_NOT_FOUND) of
		    0 -> 
			%% Incorrect data has been found, error sent to the logger.
			?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
					   {server,?LSE_LOOP_NAME},
					   {symbol,Symbol},
					   {symbol_cannot_be_fetched}},
			exit(normal);
		    _ ->
			%% Otherwise correct data is found. A small bit of JSON is wrapped around the data
			%% And it is sent to the server.
			Front = "{\"Data\": ",
			?BATCHER_LOOP_NAME!{self(), Symbol,Front ++ Data ++ "}", ?SERVER,Type},
			receive
			    {ok,?BATCHER_LOOP_NAME} ->
				?LOGGER_LOOP_NAME!{success,{timestamp,erlang:localtime()},
						   {server,?LSE_LOOP_NAME},
						   {symbol,Symbol},
						   {data_sent_to_batcher}},
				exit(normal)
			after 3000 ->
				exit(normal)
			end
		end;
	    %% Catches errors either from a closed connection or
	    %% a failed connection to a source.
	    {error,socket_closed_remotely} ->
		?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
			{server,?LSE_LOOP_NAME},
			{symbol,Symbol},
			{retry,Try+1},
			{socket_closed_remotely}},
		timer:sleep(crypto:rand_uniform(5,120)*1000),
		fetch(Symbol,Try+1,Query,Type);
	    {error,{failed_connect,_}} -> 
		?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
				   {server,?LSE_LOOP_NAME},
				   {symbol,Symbol},
				   {retry,Try+1},
				   {connection_failed}},
		timer:sleep(crypto:rand_uniform(5,120)*1000),
		fetch(Symbol,Try+1,Query,Type);
	    Other -> io:format("ERROR: ~p~n",[Other])
	end


    catch
	_ -> io:format("Unexpected error ~n",[]),
	     exit(normal)
    end;
fetch(Symbol,Try,_Query,_type) ->
    ?LOGGER_LOOP_NAME!{{timestamp,erlang:localtime()},
	    {server,?LSE_LOOP_NAME},
	    {symbol,Symbol},
	    {retries,Try},
	    {timeout}},
    exit(normal).
 
