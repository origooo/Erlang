%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2013 by Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(ndax).

-export([lookup/0,stock/1]).
-define(SYMBOLS, ["AAPL","GOOG","AEHR","FLWS","FCTY","FCCY","SRCE","FUBC","VNET","JOBS","EGHT","AVHI","SHLM","AAON","ASTM","ABAX","ABMD","AXAS","ACTG","ACHC","ACAD","ACST","AXDX","XLRN","ACCL","ANCX","ARAY","ACRX","ACET","ACHN","ACIW","ACNB","ACOR","ACFN","ACTS","ACPW","ATVI","BIRT","ACUR","ACXM","ADUS","ADEP"]).

lookup() ->
    run(?SYMBOLS).

run([]) ->
    ndax_lookup_complete;
run([H|T]) ->
    stock(H),
    %spawn(ndax,stock,[H]),
    run(T).

stock(Symbol) ->
    %io:format("\t NASDAQ: Fetching stock info... \n"),
    Query=lists:concat(["http://dev.markitondemand.com/Api/Quote/jsonp?symbol=",Symbol,"&callback=myFunction"]),
    {ok,{_,_,X}}=httpc:request(get,{Query,[]},[],[]),
    Length=string:len(X),
    Data=[string:substr(X,12,Length-12)],
    io:format(Data).
