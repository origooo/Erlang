%%% @author Patrik Bäckström
%%% @doc stocksadapa_app is the application file for the stock fetcher app
%%%
%%% @end 
%%%-------------------------------------------------------------------
-module(stocksadapa_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
	head_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
