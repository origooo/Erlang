%%% @author John <john@john-K55VD> & Carl Berglund
%%% @copyright (C) 2013, John
%%% @doc symbols is the combination of 2 previous modules
%%% which are only used for the collection of stock market symbols
%%% @end
%%% Created : 17 Oct 2013 by John <john@john-K55VD> 

-module(symbols).
-export([get_lse/0, get_nasdaq/0, loop/0, get_stock/1, get_dax/0, to_tuples/2]).
-include("definitions.hrl").

loop() ->
    receive
	
	{populate, ?DATABASE_LSE_SYMBOLS, From} ->	    
	    get_lse(),
	    From ! {ok, all_uploaded};
	{populate, ?DATABASE_NASDAQ_SYMBOLS, From} ->
	    get_nasdaq(),
	    From ! {ok, all_uploaded};
	{populate, ?DATABASE_DAX_SYMBOLS, From} ->
	    get_dax(),
	    From ! {ok, all_uploaded};
	{populate, _, From} ->
	    From ! {ok, all_uploaded}
    end.


%%--------------------------------------------------------------------
%% @doc get_lse/0 gets the symbols from the designated url for the LSE
%% @end
%%--------------------------------------------------------------------


get_lse()->
    
    {ok, {_Version, _Headers, Body}} = httpc:request(get, {"http://www.myinvestorshub.com/tmp/List525fce3a079959.73487651.csv", []}, [], []),
    Output = parse_symbols(Body),
    uploader(Output, ?DATABASE_LSE_SYMBOLS).

%Test = "Maruwa Co., Ltd.,MAW.L,LSE,United Kingdom\nMax Property Group PLC,MAX.L,LSE,United Kingdom\n".

%%--------------------------------------------------------------------
%% @doc parse_symbols takes the list and parses the symbols out of it.
%% @spec parse_symbols(list()) -> list()
%% @end
%%--------------------------------------------------------------------

parse_symbols([]) -> [];
parse_symbols(Body)->

    {Name, L1} = symbol(Body, []),
    {Symbol, L2} = symbol(L1, []),
    {_Market, L3} = symbol(L2, []),
    {_Country, L4} = symbol(L3, []),
    [{Symbol, Name} | parse_symbols(L4)].

%%--------------------------------------------------------------------
%% @doc symbol/2 pattern matches out characters we do and don't want.
%% they are then added to an accumilator which is returned when a specific
%% symbol is found.
%% @spec symbol(list(), list()) -> list()
%% @end
%%--------------------------------------------------------------------


symbol([$\n|T], Acc)->
    {Acc, T};
symbol([$,|T], Acc) when Acc =:= " Ltd."->
    symbol(T, []);
symbol([$,|T], Acc) when Acc =:= " Inc."->
    symbol(T, []);
symbol([$,|T], Acc) ->
    {Acc, T};
symbol([H|T], Acc) ->
    symbol(T, Acc ++ [H]);
symbol([], Acc) -> {Acc, []}.

%%--------------------------------------------------------------------
%% @doc uploader/2 takes a list of symbols and uploads them to couchdb
%% @spec uploader(list(), atom()) -> tuple()
%% @end
%%--------------------------------------------------------------------


uploader([], _) -> {ok, all_uploaded};
uploader([{Symbol, Name}|T], Database) ->

    URL = ?DATABASE_URL,
    %Database = ?DATABASE_LSE_SYMBOLS,
    Json= "{\"Symbol\":"++"\""++Symbol++"\""++","++
	   "\"Name\":"++"\""++Name++"\""++"}",
    httpc:request(put, {URL++Database++Symbol, [],[], Json}, [], []),
    uploader(T, Database).

%%--------------------------------------------------------------------
%% @doc get_nasdaq/0 starts the parse_file function with a specfied url
%% to where the symbols can be downloaded.
%% @spec get_nasdaq() -> list()
%% @end
%%--------------------------------------------------------------------

get_nasdaq()->
    parse_file( "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download", ?DATABASE_NASDAQ_SYMBOLS).
    %parse_file( "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=amex&render=download", "adapa_symbols_amex"),
    %parse_file( "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download", "adapa_symbols_nyse").

%%--------------------------------------------------------------------
%% @doc parse_file/2 takes the csv from the website and saves it
%% to ndax_symbols.txt.
%% @spec parse_file(string(), list()) -> list()
%% @end
%%--------------------------------------------------------------------


parse_file(URL, DB)->
    {ok, {_,_,Data}}=httpc:request(get,{URL,[]},[],[]),
    file:write_file("ndax_symbols.txt", Data),
    {ok, File} = file:open("ndax_symbols.txt", []),
    {ok, Symbols} = file:read_line(File),
    try  parse(File, [], Symbols, DB) of
	 _List->
	 ok
	catch
	    []-> ohwell
	after file:close(File)
    end.

%%--------------------------------------------------------------------
%% @doc parse/4 handles the actual parsing of the file
%% @spec parse(file(), list(), list(), list()) -> list()
%% @end
%%--------------------------------------------------------------------

    
parse(File, Acc, Symbols, DB)->
        case file:read_line(File) of
	eof -> end_brackets(end_brackets(Acc, "Data"), "Symbols");
        {ok, Line} -> Name = get_that_symbol(Line),
 Output = lists:concat(brackets(jsonify(break_line(Symbols),break_line(Line)))),
		      URL = ?DATABASE_URL,
		      {ok, {_,_,_Okay}} = httpc:request(put,{URL++DB++"/"++Name, [], "application/json", Output}, [{version, "HTTP/1.0"}], []),
		      parse(File, Acc++brackets(Output), Symbols, DB)
	end.
    

%%--------------------------------------------------------------------
%% @doc break_line/1 breaks lines and adds a backslash.
%% @spec break_line(string()) -> string()
%% @end
%%--------------------------------------------------------------------

break_line(Line)->
   string:tokens(Line, "\"").

%%--------------------------------------------------------------------
%% @doc get_that_symbol/1 gets symbols and performs breakline on them
%% @spec get_that_symbol(list())-> string()
%% @end
%%--------------------------------------------------------------------


get_that_symbol(Line)->
    get_symbol(break_line(Line)).
get_symbol([H|_T])->
    H.

%%--------------------------------------------------------------------
%% @doc jsonify/2 jsonifies the list.
%% @spec jsonify(list(), list()) -> list()
%% @end
%%--------------------------------------------------------------------


jsonify([], [])->
    [];
jsonify([H|T],[H2|T2])when [H] == [","]; [H2] == [","]->
    case T of
	[]->
	    [];
	_->
	    [","| jsonify(T,T2)]
    end;

jsonify([_H|T],[_H2|T2])when T == [], T2 == []->
    [];
jsonify([H|T], [H2|T2]) ->
    [lists:concat(["\"", H,"\"", ":", "\"", H2,"\""]) | jsonify(T,T2)].

%%--------------------------------------------------------------------
%% @doc brackets/1 adds brackets to a line.
%% @spec brackets(string())-> list()
%% @end
%%--------------------------------------------------------------------


brackets(Line)->
    lists:concat([["{"] , Line ,["}"]]).

%%--------------------------------------------------------------------
%% @doc end_brackets/2 adds end brackets to a word and a line
%% @spec end_brackets(string(), string()) -> list()
%% @end
%%--------------------------------------------------------------------


end_brackets(Line, Word)->
   lists:concat(["{", "\"", Word, "\"", ":",Line, "}"]).
    


%%--------------------------------------------------------------------
%% @doc
%% get/1 performs a HTTP GET request to the database, returning all of
%% the symbols currently being stored. 
%% Takes a tuple of {atom(), list()} where the atom should be
%% the name of the database you wish to use, i.e. nasdaq
%% @spec get_stock(tuple()) -> string()
%% @end
%%--------------------------------------------------------------------

get_stock({Name,InetsProfile,Acc}) ->

    NewName = case Name of
	
		  nasdaq_server -> ?DATABASE_NASDAQ_SYMBOLS;
		  lse_server    -> ?DATABASE_LSE_SYMBOLS;
		  dax_server    -> ?DATABASE_DAX_SYMBOLS
	      end,
    
    {ok, {{_, _, _}, _, Body}} = 
	httpc:request(get,{lists:concat([?DATABASE_URL, NewName, ?DATABASE_VIEW_SYMBOLS]),[]}, 
		      [],[],InetsProfile),

	
    decode(Body, Acc).

%%--------------------------------------------------------------------
%% @doc
%% decode/2 performs a mochiweb decode upon the json string 
%% which is returned from get/1. This record is then sent over to
%% strip to get out the symbols.
%% @spec decode(string(), list()) -> record()
%% @end
%%--------------------------------------------------------------------

decode(Json, Acc) ->
    {struct, JsonData} = mochijson:decode(Json),
    strip(JsonData, Acc).

%% Decoded JsonData has the following format.
%% [{"total_rows", N},
%%  {"offset"}, N},
%%  {"rows",
%%  {array, [{struct, [{"id", "SYMBOL-WE-WANT"}, _,_},
%%           {struct, [...]}
%%  One array entry for each value stored in the database.

%%--------------------------------------------------------------------
%% @doc
%% strip/2 recurses through the record structure and returns a list of
%% all the symbols retrieved from get/1. The list is returned then 
%% returned in a tuple, stating {ok, List}.
%% @strip(list(), list()) -> tuple()
%% @end
%%--------------------------------------------------------------------

strip([_TotalRows, _Offset, Array], Acc) ->
    strip(Array, Acc);

strip({_Title, {array, Data}}, Acc) ->
    strip(Data, Acc);

strip([{struct, [{_Id, Symbol}, _Key, _Value]} | T], Acc) ->
    strip(T, [Symbol | Acc]);

strip({struct, [{_Id, Symbol}, _Key, _Value]}, Acc) ->
    strip([], [Symbol | Acc]);

strip([], Acc) ->
    {ok, lists:reverse(Acc)}.
    %--Uncomment the below to have the list in reverse order
    %Acc.


get_dax() ->
    {ok, Binary}=file:read_file("ebin/symbols/dax_symbols.txt"),
    Binary2=re:split(Binary, "\n"),
    Out=to_tuples(Binary2, []),
    uploader(Out, ?DATABASE_DAX_SYMBOLS).

to_tuples([<<>>|_], Acc)->Acc;
to_tuples([H|T], Acc) ->
    [Symbol, Name] = re:split(H, ","),
    to_tuples(T, [{binary_to_list(Symbol), binary_to_list(Name)}|Acc]).
    
