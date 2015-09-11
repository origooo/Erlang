%%% @author John <john@john-K55VD>
%%% @copyright (C) 2013, John
%%% @doc db_checker checks if the databases exist on CouchDB
%%% If they don't then they are created. It confirms that
%%% also contain the correct views / update handlers.
%%% @end
%%% Created :  6 Nov 2013 by John <john@john-K55VD>

-module(db_checker).
-include("definitions.hrl").
-export([checker/0,drop_dbs/0, init/0, check_loop/1]).

%%--------------------------------------------------------------------
%% @doc init/0 registers checker to the atom checker.
%% @spec init() -> pid()
%% @end
%%--------------------------------------------------------------------

init() ->
    
    register(checker, spawn(fun()-> check_loop(0) end)).

%%--------------------------------------------------------------------
%% @doc checker/0 is the exported method called by other modules wishing to
%% register a version of checker. Inets is also started along with the
%% appropiate inets_profile.
%% @spec checker() -> tuple()
%% @end
%%--------------------------------------------------------------------


checker()->
    
    init(),
    inets:start(httpc, [{profile, ?DB_CHECKER_INETS_PROFILE}]).

%%--------------------------------------------------------------------
%% @doc check/2 is used to check that a particular datase or page exists
%% on the databse at this current time. It returns the query, the reply and 
%% a 1 for completion, a 0 for fail.
%% @spec check(string(), atom()) -> tuple()
%% @end
%%--------------------------------------------------------------------


check(Check, Method) ->
    
    Reply = case Method of
	
	get ->   httpc:request(Method,
				      {?DATABASE_URL++Check, []}, [],[], ?DB_CHECKER_INETS_PROFILE);
	put ->   httpc:request(Method,
				      {?DATABASE_URL++Check++"test",
				       [], [],?TEST_JSON}, [],[], ?DB_CHECKER_INETS_PROFILE)
    end,
    
    case Reply of
	
	{ok,_} -> checker ! {Check, Reply, 1};
	{error,_} -> checker ! {Check, Reply, 0}
    end.

%%--------------------------------------------------------------------
%% @doc check_loop/1 is the waiting message loop that the checker utilises.
%% It waits for messages and then acts upon them accordingly. It also keeps track
%% of the amount of attempted / failed operations.
%% @spec check_loop(integer()) -> atom()
%% @end
%%--------------------------------------------------------------------

check_loop(Passes)->
    
    receive 
	
	{check,dbs, From}       -> 
	    database_checks(),
	    From ! {ok, completed},
	    check_loop(Passes);
	{check,symbols, From} ->
	    symboldb_checks(),
	    From ! {ok, completed},
	    check_loop(Passes);
	{check,update, From}    -> 
	    update_checks(),
	    From ! {ok, completed},
	    check_loop(Passes);
	{check,views, From}     -> 
	    view_checks(),
	    From ! {ok, completed},	    
	    check_loop(Passes);
	{drop,symbols, From} ->
	    drop_dbs(),
	    From ! {ok, dropped},
	    check_loop(Passes);   
   
	
	%% Couch will probably be down if this comes
	{_,{error, _}, _Pass} -> 
	    io:format("Stopping, Couch is not running.");
	%% Page exists.
	{Check,{ok,{{_,200,"OK"},_,_}}, Pass} -> 
	    io:format("ok, ~p exists\n", [Check]),
	    check_loop(Passes+Pass);
	%% DB exists, but view does not.
	{Check,{ok,{{_, 404,"Object Not Found"},_,Body}},Pass} when length(Body) =:= 41 -> 
	    io:format("Db exists but no view exists for ~p, Creating..\n", [Check]),
	    create_view(Check),
	    check_loop(Passes+Pass);
	%% Database does not exist.
	{Check,{ok,{{_, 404,"Object Not Found"},_,Body}}, Pass} when length(Body) =:= 44 -> 
	    io:format("error, ~p not found, Creating...\n", [Check]),
	    create_database(Check),
	    check_loop(Passes+Pass);
	%% Update works.
	{Check,{ok,{{_,201,"Created"},_,_}}, Pass} ->
	    io:format("ok, ~p exists\n",[Check]),
	    check_loop(Passes+Pass);

	stop  -> 
	    inets:stop(httpc,[{profile, ?DB_CHECKER_INETS_PROFILE}]),
	    {ok, stopped};

	{finished, From} -> 

	    From ! {ok, Passes, completed},
	    checker!stop

    end.


%%--------------------------------------------------------------------
%% @doc database_checks/0 checks the database in a given list.
%% @spec database_checks() -> check_list(list(), atom())
%% @end
%%--------------------------------------------------------------------

database_checks() ->

    List = [
	    ?DATABASE_NAME_NASDAQ,
    	    ?DATABASE_NAME_LSE,
    	    ?DATABASE_NAME_DAX,
    	    ?DATABASE_NAME_USERS
	    ],

    check_list(List, get).

%%--------------------------------------------------------------------
%% @doc symboldb_checks/0 checks the database in a given list.
%% @spec symboldb_checks() -> check_list(list(), atom())
%% @end
%%--------------------------------------------------------------------

symboldb_checks() ->
    
    List = [
	    ?DATABASE_NASDAQ_SYMBOLS,
	    ?DATABASE_LSE_SYMBOLS,
	    ?DATABASE_DAX_SYMBOLS
	   ],
    check_list(List, get).

%%--------------------------------------------------------------------
%% @doc update_checks/0 checks the update handler in a given list.
%% @spec update_checks() -> check_list(list(), atom())
%% @end
%%--------------------------------------------------------------------


update_checks() ->

    List = [
	    ?DATABASE_NAME_NASDAQ++?DATABASE_UPDATE_HANDLER,
	    ?DATABASE_NAME_LSE++?DATABASE_UPDATE_HANDLER,
	    ?DATABASE_NAME_DAX++?DATABASE_UPDATE_HANDLER,
	    ?DATABASE_NAME_USERS++?USER_UPDATE_HANDLER_URL
	   ],

    check_list(List, put).

%%--------------------------------------------------------------------
%% @doc view_checks/0 checks the symbol view in a given list.
%% @spec view_checks() -> check_list(list(), atom())
%% @end
%%--------------------------------------------------------------------

view_checks() ->

    List = [
	    ?DATABASE_NASDAQ_SYMBOLS++?DATABASE_VIEW_SYMBOLS,
	    ?DATABASE_LSE_SYMBOLS++?DATABASE_VIEW_SYMBOLS,
	    ?DATABASE_DAX_SYMBOLS++?DATABASE_VIEW_SYMBOLS,
	    ?DATABASE_NAME_USERS++?DATABASE_VIEW_USERS,
	    ?DATABASE_NAME_NASDAQ++?DATABASE_VIEW_LATEST,
	    ?DATABASE_NAME_LSE++?DATABASE_VIEW_LATEST,
	    ?DATABASE_NAME_DAX++?DATABASE_VIEW_LATEST
	    ],
    check_list(List, get).

%%--------------------------------------------------------------------
%% @doc check_list/0 applies a function to a list.
%% @spec check_list(list(), atom()) -> list()
%% @end
%%--------------------------------------------------------------------

check_list(List, Method) ->
    
    Fun = fun(X) -> check(X, Method) end,
    lists:foreach(Fun, List).

%%--------------------------------------------------------------------
%% @doc create_database/1 creates a database on couchdb from the given
%% name.
%% @spec create_database(string()) -> tuple()
%% @end
%%--------------------------------------------------------------------


create_database(Name) ->
    

    Reply = httpc:request(put, {?DATABASE_URL++Name, [], [], []}, [],[], ?DB_CHECKER_INETS_PROFILE),
    case Reply of
	
	{ok,{{_,201,"Created"},_,_}} ->  
	    io:format("Created ~p\n", [Name]),
	    populate(Name);

	_ ->   io:format("Error creating ~p\n", [Name])

    end.

%%--------------------------------------------------------------------
%% @doc create_view/1 creates a specific view for a particular datbase.
%% the view is loaded from a local JSON file and sent to the database.
%% @spec create_view(string()) -> tuple()
%% @end
%%--------------------------------------------------------------------


create_view(Name) ->
        

    {Type, URL} = get_type(Name),

    case file:read_file(Type) of
		
	{ok, Binary} -> 
	    httpc:request(put,{lists:concat(
				 [?DATABASE_URL, URL]), [], [], Binary}, [],[],?DB_CHECKER_INETS_PROFILE),
	    io:format("ok, created ~p\n", [Name]);
	
	{error, Reason} -> io:format("unable to open ~p file. ~p\n", 
				     [Name, Reason])

    end.

%%--------------------------------------------------------------------
%% @doc populate/1 populates the empty symbol databases with their symbols
%% this action is asynchronos so the caller must wait for it to be finished.
%% @spec populate(string()) -> tuple()
%% @end
%%--------------------------------------------------------------------

populate(Name) ->
    
    Pid = spawn(fun()->  symbols:loop() end),
    Pid ! {populate, Name, self()},
    receive
	{ok, all_uploaded} -> 
	    io:format("~p Symbols populated\n", [Name])
    end.


%%--------------------------------------------------------------------
%% @doc get_type/1 takes the calling server and gives back the correct
%% format for operations in the create_view function.
%% @spec get_type(string()) -> tuple()
%% @end
%%--------------------------------------------------------------------

get_type(Name) ->
    
    case Name of
	
	?DATABASE_NAME_NASDAQ++?DATABASE_UPDATE_HANDLER  -> {?UPDATE_HANDLER,?DATABASE_NAME_NASDAQ++?UPDATE_HANDLER_URL};
	?DATABASE_NAME_LSE++?DATABASE_UPDATE_HANDLER     -> {?UPDATE_HANDLER,?DATABASE_NAME_LSE++?UPDATE_HANDLER_URL};
	?DATABASE_NAME_DAX++?DATABASE_UPDATE_HANDLER     -> {?UPDATE_HANDLER,?DATABASE_NAME_DAX++?UPDATE_HANDLER_URL};
	?DATABASE_NASDAQ_SYMBOLS++?DATABASE_VIEW_SYMBOLS -> {?SYMBOL_VIEW,?DATABASE_NASDAQ_SYMBOLS++?SYMBOL_VIEW_URL};
	?DATABASE_LSE_SYMBOLS++?DATABASE_VIEW_SYMBOLS    -> {?SYMBOL_VIEW,?DATABASE_LSE_SYMBOLS++?SYMBOL_VIEW_URL};
	?DATABASE_DAX_SYMBOLS++?DATABASE_VIEW_SYMBOLS    -> {?SYMBOL_VIEW,?DATABASE_DAX_SYMBOLS++?SYMBOL_VIEW_URL};
	?DATABASE_NAME_USERS++?DATABASE_VIEW_USERS       -> {?USER_VIEW,?DATABASE_NAME_USERS++?USER_VIEW_URL};
	?DATABASE_NAME_USERS++?USER_UPDATE_HANDLER_URL   -> {?USER_UPDATE_HANDLER,?DATABASE_NAME_USERS++?DATABASE_VIEW_USERS};
	?DATABASE_NAME_NASDAQ++?DATABASE_VIEW_LATEST     -> {?LATEST_DATA,?DATABASE_NAME_NASDAQ++?LATEST_DATA_VIEW_URL};
	?DATABASE_NAME_LSE++?DATABASE_VIEW_LATEST        -> {?LATEST_DATA,?DATABASE_NAME_LSE++?LATEST_DATA_VIEW_URL};
	?DATABASE_NAME_DAX++?DATABASE_VIEW_LATEST        -> {?LATEST_DATA,?DATABASE_NAME_DAX++?LATEST_DATA_VIEW_URL}


    end.

%%--------------------------------------------------------------------
%% @doc drop_dbs/0 drops a list of database names from the database
%% @spec drop_dbs() -> list()
%% @end
%%--------------------------------------------------------------------

	
drop_dbs() ->
    
    List = [
	    ?DATABASE_NASDAQ_SYMBOLS,
	    ?DATABASE_LSE_SYMBOLS,
	    ?DATABASE_DAX_SYMBOLS
	   ],
    
    Fun = fun(X) -> httpc:request(delete, {lists:concat([?DATABASE_URL,X]), []},[], [], ?DB_CHECKER_INETS_PROFILE) end,
    lists:foreach(Fun, List).
