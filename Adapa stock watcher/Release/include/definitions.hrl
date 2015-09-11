%%%-------------------------------------------------------------------
%%% @author Patrik BÃ¤ckstrÃ¶m <origooo@origoMBP.local>
%%% @copyright (C) 2013, Patrik BÃ¤ckstrÃ¶m
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2013 by Patrik BÃ¤ckstrÃ¶m <origooo@origoMBP.local>
%%%-------------------------------------------------------------------
%-module(definitions).

-define(SEC,1000).
-define(MIN,1000*60).
-define(HOUR,1000*60*60).
-define(DAY,1000*60*60*24).
-define(SLEEP_SHORT,5*1000).
-define(SLEEP_LONG,10*1000).
-define(MAX_RETRIES,3).


%%--------------------------------------------------------
%% Database definitions
%% Shall hold:
%%        URL to database,
%%        Name of database for each individual database used,
%%        Name/URL to views used by CoachDB to insert stocks and symbols etc.
%%
%%--------------------------------------------------------

%% Remote database URL.
-define(DATABASE_URL,            "http://83.254.83.56:8002/").
%-define(DATABASE_URL,            "http://127.0.0.1:5984/").
-define(DATABASE_NAME_NASDAQ,    "nasdaq_stocks/").
-define(DATABASE_NASDAQ_SYMBOLS, "nasdaq_symbols/").
-define(DATABASE_NAME_LSE,       "lse_stocks/").
-define(DATABASE_LSE_SYMBOLS,    "lse_symbols/").
-define(DATABASE_NAME_DAX,       "dax_stocks/").
-define(DATABASE_DAX_SYMBOLS,    "dax_symbols/").
-define(DATABASE_NAME_USERS,     "users/").

%-define(TEST_JSON, "{\"integer\": 0, \"string\": \"0\"}").
-define(TEST_JSON, "{\"Data\": {\"Name\": \"test\",\"Symbol\": \"est\",\"LastPrice\": 0,\"Change\": 0,\"ChangePercent\": \"0\",\"High\": 0,\"Low\": 0,\"Open\": 0, \"Close\":0, \"Volume\": 9000}}").

-define(UPDATE_HANDLER,          "ebin/couch_views/update_handler.json").
-define(SYMBOL_VIEW,             "ebin/couch_views/symbol_view.json").
-define(USER_UPDATE_HANDLER,     "ebin/couch_views/user_update_handler.json").
-define(USER_VIEW,               "ebin/couch_views/user_views.json").
-define(LATEST_DATA,             "ebin/couch_views/latestData.json").

-define(UPDATE_HANDLER_URL,      "/_design/update").
-define(SYMBOL_VIEW_URL,         "/_design/symbols").
-define(USER_VIEW_URL,           "/_design/user_info").
-define(LATEST_DATA_VIEW_URL,    "/_design/latestData").

-define(DATABASE_UPDATE_HANDLER, "_design/update/_update/Update/").
-define(DATABASE_STORE_HIST,     "_design/update/_update/Historical/").
-define(USER_UPDATE_HANDLER_URL, "_design/update/_update/update_user/").
-define(DATABASE_VIEW_USERS,     "_design/user_info/_view/get_user/").
-define(DATABASE_VIEW_SYMBOLS,   "_design/symbols/_view/get_symbols/").
-define(DATABASE_VIEW_LATEST,    "_design/latestData/_view/latestData/").


%%--------------------------------------------------------
%% Nasdaq definitions
%% Shall hold: (NOT COMPLETE)
%%        Specified minute on which the fetcher should be run,
%%        The first whole hour the stock market is open,
%%        The last whole hour the stock market is open,
%%        The timezone difference between erlangs local time and the market,
%%        URL to the actual source used to fetch data,
%%        Significant/unique part of error message appearing when symbols do not exist (most likely different from source to source).
%%
%%--------------------------------------------------------
-define(NASDAQ_FETCH_MIN,10).
-define(NASDAQ_OPEN_HOUR,10).
-define(NASDAQ_CLOSE_HOUR,17). %Changed to 17 so that we can get a close value.
-define(NASDAQ_TIMEZONE_ADJUSTMENT,-6).
-define(NASDAQ_SOURCE_URL,"http://dev.markitondemand.com/Api/Quote/jsonp?symbol=").
-define(NASDAQ_SYMBOL_NOT_FOUND,"No symbol matches found for").

-define(NASDAQ_INETS_PROFILE,nasdaq_inets_profile).
-define(NASDAQ_LOOP_NAME,nasdaq_loop).


%%--------------------------------------------------------
%% DAX definitions
%% Shall hold: (NOT COMPLETE)
%%        Specified minute on which the fetcher should be run,
%%        The first whole hour the stock market is open,
%%        The last whole hour the stock market is open,
%%        The timezone difference between erlangs local time and the market,
%%        URL to the actual source used to fetch data,
%%        Significant/unique part of error message appearing when symbols do not exist (most likely different from source to source).
%%
%%--------------------------------------------------------
-define(DAX_FETCH_MIN,20).
-define(DAX_OPEN_HOUR,9).
-define(DAX_CLOSE_HOUR,19). %Changed to 19 so that we can get a close value.
-define(DAX_TIMEZONE_ADJUSTMENT,-0).
-define(DAX_SOURCE_URL1,"http://finance.yahoo.com/d/quotes.csv?s=").
-define(DAX_SOURCE_URL2,"&f=nsl1c1p2hgovlp").
-define(DAX_SYMBOL_NOT_FOUND,"N/A,N/A,N/A,N/A,N/A").

-define(DAX_INETS_PROFILE,dax_inets_profile).
-define(DAX_LOOP_NAME,dax_loop).
%%httpc:request(get,{”http://finance.yahoo.com/d/quotes.csv?s=DAI.DE&f=nsl1c1p2hgol”,[]},[],[]).
%%--------------------------------------------------------
%% LSE definitions
%% Shall hold: (NOT COMPLETE)
%%        Specified minute on which the fetcher should be run,
%%        The first whole hour the stock market is open,
%%        The last whole hour the stock market is open,
%%        The timezone difference between erlangs local time and the market,
%%        URL to the actual source used to fetch data,
%%        Significant/unique part of error message appearing when symbols do not exist (most likely different from source to source).
%%
%%--------------------------------------------------------
-define(LSE_FETCH_MIN,30).
-define(LSE_OPEN_HOUR,9).
-define(LSE_CLOSE_HOUR,18). %Changed to 18 so that we can get a close value.
-define(LSE_TIMEZONE_ADJUSTMENT,-1).
-define(LSE_SOURCE_URL2, "https://script.google.com/macros/s/AKfycbxE2cms8pQqQvPKO-0kWj-F_VI_gx3yv0WInAaOn2wMpYYodWTU/exec?stock=").
-define(LSE_SOURCE_URL1, "https://script.google.com/macros/s/AKfycbzEvuuQl4jkrbPCz7hf9Zv4nvIOzqAkBxL1ixslLBxmSEhksQM/exec?stock=").
-define(LSE_SYMBOL_NOT_FOUND,"Low").

-define(LSE_INETS_PROFILE,lse_inets_profile).
-define(LSE_LOOP_NAME,lse_loop).

%%--------------------------------------------------------
%% BATCHER definitions
%%
%%--------------------------------------------------------
-define(BATCHER_INETS_PROFILE,batcher_inets_profile).
-define(BATCHER_LOOP_NAME,batcher).

%%--------------------------------------------------------
%% LOGGER definitions
%%
%%--------------------------------------------------------
-define(LOGGER_LOOP_NAME,logger).

%%--------------------------------------------------------
%% Symbol server definitions
%%
%%--------------------------------------------------------
-define(SYMBOL_SERVER_LOOP_NAME, symbol_server_loop).
-define(SYMBOL_SERVER_INETS_PROFILE, symbol_server_inets_profile).

%%--------------------------------------------------------
%% db_checker definitions
%%
%%--------------------------------------------------------
-define(DB_CHECKER_INETS_PROFILE, db_checker_inets_profile).
