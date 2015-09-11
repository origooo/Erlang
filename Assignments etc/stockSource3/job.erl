%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-204174.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created :  4 Oct 2013 by Patrik Bäckström <origooo@dhcp-204174.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(job).

%% Internal functions
-export([job/1]).

-define(DELAY1HOUR,1000*60*60).
-define(DELAY1DAY,1000*60*60*24).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
job({hourly,{Source,From,To},LocalTimeDiff}) ->
    {{_,_,_},{H,_,_}}=erlang:localtime(),
    case H+LocalTimeDiff>=From of
	true when H+LocalTimeDiff<To ->
	    spawn(Source,lookup,[]),
	    timer:sleep(?DELAY1HOUR),
	    job({hour,{Source,From,To},LocalTimeDiff});
	_Other ->
	    timer:sleep(?DELAY1HOUR),
	    job({hour,{Source,From,To},LocalTimeDiff})
    end;
%% job({daily,{Source,Hour}}) ->
%%     ok;
%% job({test,{Source}}) ->
%%     ok;
job(_) ->
    ok.
