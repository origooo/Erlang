%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2013 by Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(ndax).

%% Internal functions
-export([lookup/0,stock/1,localtime/1]).

-define(SYMBOLS, ["AAPL","GOOG","AEHR","FLWS","FCTY","FCCY","SRCE","FUBC","VNET","JOBS","EGHT","AVHI","SHLM","AAON","ASTM","ABAX","ABMD","AXAS","ACTG","ACHC","ACAD","ACST","AXDX","XLRN","ACCL","ANCX","ARAY","ACRX","ACET","ACHN","ACIW","ACNB","ACOR","ACFN","ACTS","ACPW","ATVI","BIRT","ACUR","ACXM","ADUS","ADEP"]).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
lookup() ->
    run(?SYMBOLS).

run([]) ->
    ndax_lookup_complete;
run([H|T]) ->
    spawn(ndax,stock,[H]),
    %stock(H),
    run(T).

stock(Symbol) ->
    Query=lists:concat(["http://dev.markitondemand.com/Api/Quote/jsonp?symbol=",Symbol,"&callback=myFunction"]),
    {ok,{_,_,X}}=httpc:request(get,{Query,[]},[],[]),
    Length=string:len(X),
    Data=string:substr(X,12,Length-12),
    
    %%case is_valid(Data) of 
    %% 	 true -> io:format("send to couch",[]); %send data to couch
    %%   false -> io:format("send to logfile",[]) %send data to error document in couch
    %% end.
    
    CouchQuery=lists:concat(["http://127.0.0.1:5984/adapa_term3_bi/",Symbol]),
    httpc:request(put,{CouchQuery,[],"application/json",Data},[{version,"HTTP/1.0"}],[]).


%% is_valid([_H=$M|_T]) -> false;
%% is_valid([_H=$D|_T]) -> true;
%% is_valid([_H|T]) -> is_valid(T).

localtime({{Y,Mo,D},{H,M,S}}) ->
    [Y-Mo,"-",D,", ",H,":",M,":",S].


%httpc:request(get,{"http://dev.markitondemand.com/Api/Quote/jsonp?symbol=AAPL&callback=myFunction",[]},[],[])
