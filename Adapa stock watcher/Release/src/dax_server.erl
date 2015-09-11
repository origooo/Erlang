%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-194126.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc Alike the other 2 servers, this collects data from the German
%%% DAX exchange sending a the data receieved to the batcher server via a message
%%% @end
%%% Created : 31 Oct 2013 by Patrik Bäckström <origooo@dhcp-194126.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(dax_server).
-include("definitions.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0, remove/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal functions
-export([loop/0,fetch/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(SYMBOLS,["ADS.DE","ALV.DE","BAS.DE","BAYN.DE","BEI.DE","BMW.DE","CBK.DE","DAI.DE","DB1.DE","DBK.DE","DPW.DE","DTE.DE","EOAN.DE","FME.DE","FRE.DE","HEI.DE","IFX.DE","LHA.DE","LIN.DE","MAN.DE","MEO.DE","MRK.DE","MUV2.DE","RWE.DE","SAP.DE","SDF.DE","SIE.DE","TKA.DE","VOW3.DE"]).

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
    inets:start(httpc,[{profile,?DAX_INETS_PROFILE}]),
    process_flag(trap_exit,true),
    register(?DAX_LOOP_NAME,spawn_link(fun() -> loop() end)),
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
    inets:stop(httpc,?DAX_INETS_PROFILE),
    case whereis(?DAX_LOOP_NAME) of
	undefined -> ok;
	_ -> exit(whereis(?DAX_LOOP_NAME),Reason)
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
%% @doc Main loop, similar to the other loops. It waits for the time
%% defined in the definitions.hrl file. Once the time is correct it
%% loops through all symbols in its list and spawns a function process
%% per symbol in the list. It is then sent to fetch/3
%% @spec loop() -> any()
%% @end
%%--------------------------------------------------------------------


loop() ->
    {{_,_,_},{H,M,_S}}=erlang:localtime(),
    %M=20,
    %H=10,
    Fun=fun(Symbol) -> 
		spawn(fun() -> Query=lists:concat([?DAX_SOURCE_URL1,Symbol,?DAX_SOURCE_URL2]),
			      fetch(Symbol,0,Query,normal)
		      end)
	end,

    case M of
	%% First data request, sends data to both historical and normal database.
	?DAX_FETCH_MIN when H+?DAX_TIMEZONE_ADJUSTMENT=:=?DAX_OPEN_HOUR ->
	    LastFun=fun(Symbol) -> spawn(fun() -> Query=lists:concat([?DAX_SOURCE_URL1,Symbol,?DAX_SOURCE_URL2]),
						  fetch(Symbol,0,Query,last)end) end,
	    {ok,List}=symbols:get_stock({?MODULE,?DAX_INETS_PROFILE,[]}),
	    lists:foreach(LastFun,List),
	    timer:sleep(?MIN),
	    loop();
	%% Standard request method, sends data hourly while during open hours.
	?DAX_FETCH_MIN when H+?DAX_TIMEZONE_ADJUSTMENT>=?DAX_OPEN_HOUR,
			       H+?DAX_TIMEZONE_ADJUSTMENT=<?DAX_CLOSE_HOUR ->
	    List=?SYMBOLS,
	    lists:foreach(Fun,List),
	    timer:sleep(?MIN),
	    loop();
	_ -> timer:sleep(?MIN),loop()
    end.

%%--------------------------------------------------------------------
%% @doc fetch/3 takes the symbol and amount of attempts made so far along
%% with the query. Each proccess is slept for a random amount of time between
%% with the upper time being 2 mins.
%% @spec fetch(list(), integer(), string(), atom()) -> tuple()
%% @end
%%--------------------------------------------------------------------

fetch(Symbol,Try,Query,Type) when Try < ?MAX_RETRIES ->
    timer:sleep(crypto:rand_uniform(5,1200)*100),
    try
	Result=httpc:request(get,
			    {Query,[]},
			    [],
			    [],
			    ?DAX_INETS_PROFILE),
	case Result of
	    {ok,{_,_,[]}} ->
		timer:sleep(crypto:rand_uniform(5,120)*1000),
		fetch(Symbol,Try+1,Query,Type);
	    {ok,{_,[_,_,{_,Timestamp},_,_,_],X}} ->
		%% Successful request, data is now checked agaisnt the markets
		%% reply. If it contains the SYMBOL_NOT_FOUND string then it's
		%% not a correct reply.
		case string:str(X,?DAX_SYMBOL_NOT_FOUND) of
		    % 0 indicates incorrect data.
		    0 ->
			Data=format_string(X,Timestamp),
	    	        ?BATCHER_LOOP_NAME!{self(),Symbol,Data,?SERVER,Type},
		        receive
		            {ok,?BATCHER_LOOP_NAME} -> 
				?LOGGER_LOOP_NAME!{success,{timestamp,erlang:localtime()},
					{server,?DAX_LOOP_NAME},
					{symbol,Symbol},
					{data_sent_to_batcher}},
				exit(normal)
		        after 3000 ->
				exit(normal)
		        end;
		    % Correct data is returned.
		    _ -> 
			?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
				{server,?DAX_LOOP_NAME},
				{symbol,Symbol},
				{symbol_cannot_be_fetched}},
			exit(normal)
		end;

	    %% Catches errors either from a closed connection or
	    %% a failed connection to a source.
	    {error,socket_closed_remotely} ->
		?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
			{server,?DAX_LOOP_NAME},
			{symbol,Symbol},
			{retry,Try+1},
			{socket_closed_remotely}},
		timer:sleep(crypto:rand_uniform(5,120)*1000),
		fetch(Symbol,Try+1,Query,Type);
	    {error,{failed_connect,_}} -> 
		?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
				   {server,?DAX_LOOP_NAME},
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

fetch(Symbol,Try,_Query,_) ->
    ?LOGGER_LOOP_NAME!{error,{timestamp,erlang:localtime()},
	    {server,?DAX_LOOP_NAME},
	    {symbol,Symbol},
	    {retries,Try},
	    {timeout}},
    exit(normal).

%%--------------------------------------------------------------------
%% @doc format_string/2 takes the data from the dax query along with the
%% current timestamp. It then forms it into proper JSON to be sent to the 
%% database.
%% @spec format_string(string(), string()) -> string()
%% @end
%%--------------------------------------------------------------------

format_string(Data,Timestamp) ->
    Values = re:split(Data,"[,\r\n]"),
    string:join([
		 "{\"Data\":{\"Status\":\"SUCCESS\"",
		 ",\"Name\":",binary_to_list((catch lists:nth(1,Values))),
		 ",\"Symbol\":",binary_to_list(lists:nth(2,Values)),
		 ",\"LastPrice\":",remove(binary_to_list(lists:nth(3,Values))),
		 ",\"Change\":\"",remove(binary_to_list(lists:nth(4,Values))),"\"",
		 ",\"ChangePercent\":",fixme(binary_to_list(lists:nth(5,Values))),"",
		 ",\"High\":",remove(binary_to_list(lists:nth(6,Values))),
		 ",\"Low\":",remove(binary_to_list(lists:nth(7,Values))),
		 ",\"Open\":",remove(binary_to_list(lists:nth(8,Values))),
		 ",\"Volume\":",remove(binary_to_list(lists:nth(9,Values))),
		 ",\"TimeStamp\":\"",Timestamp,"\"",
		     "}}"],"").
%%--------------------------------------------------------------------
%% @doc remove/1 removes un-wanted characters or symbols from the data
%% being parsed in format_string/3
%% @spec remove(list()) -> string()
%% @end
%%--------------------------------------------------------------------

remove(List) ->
    case lists:last(List) of
	$B -> 
	    L=length(List),
	    string:substr(List,1,L-1);
	$M -> 
	    L=length(List),
	    string:substr(List,1,L-1);
	$% ->
	    L=length(List),
	    string:substr(List,1,L-1);
	_ -> List
    end.


%%--------------------------------------------------------------------
%% @doc fixme/1 fixes a bug we were having with the binary conversion.
%% @spec fixme(list()) -> list()
%% @end
%%--------------------------------------------------------------------

fixme(List) ->	 
    [H,_|T]=re:replace(              List ,"%",""),
    binary_to_list(H)++binary_to_list(T).
