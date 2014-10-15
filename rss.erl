%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-207236.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% @author Patrik Bäckström <origooo@dhcp-207236.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2013 by Patrik Bäckström <origooo@dhcp-207236.eduroam.chalmers.se>

-module(rss).

%% load=loadXML, cP=closeProfile, sP=startProfile.
-export([init/0, load/0, cP/1, sP/1, terminate/0,reloader/1,ea/0]).
-include_lib("xmerl/include/xmerl.hrl").


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% Init/0 - 
%% @end
%%--------------------------------------------------------------------

init() ->
    inets:start().
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% startProfile/1 - starts one inets http client with the arguments as process name.
%% @end
%%--------------------------------------------------------------------

sP(Profile) ->
    {ok,Pid}=inets:start(httpc,[{profile,Profile}]),
    reloader(Pid).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% loadXML/1 - reads external XML file.
%% @end
%%--------------------------------------------------------------------

load() ->
    %{ok,{_,_,XML}}=httpc:request(get, {"http://feeds.foxnews.com/foxnews/latest", []},[], []),
    {ok,Path}=file:get_cwd(),
    %%{Result,_}=xmerl_scan:string(XML),
    %file:write_file([Path, "/rssFeed/foxnews.xml"], io_lib:fwrite("~p.\n", XML)).
    %%xmerl_sax_parser:file(XML,[]).
    %%xmerl_sax_parser:file([Path, "/rssFeed/cleanTest.xml"],[]).
    
    %{XML, _} = xmerl_scan:file([Path,"/rssFeed/cleanTest.xml"]),
    %file:write_file([Path, "/rssFeed/test.txt"], io_lib:fwrite("~p.\n", [""])),
    %ee(XML,[]).
    {ok,XML} = file:read_file([Path,"/rssFeed/cleanTest.xml"]),
    erlsom:parse(XML, Model).
    %erlsom:parse(XML, [], fun(Event, Acc) -> io:format("~p~n", [Event], Acc, end)).

%io:format("~p~n", [Event])

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% extractElements/0 - Used to extract some given elements.
%% @end
%%--------------------------------------------------------------------

ee(R,Acc) when is_record(R,xmlElement) ->
    {ok,Path}=file:get_cwd(),

    case R#xmlElement.name of
	channel -> lists:foldl(fun ee/2,Acc,R#xmlElement.content);
        item ->
            ItemData=lists:foldl(fun ee/2,[],R#xmlElement.content),
            Data=[ItemData|Acc],
	    file:write_file([Path, "/rssFeed/test.txt"], io_lib:fwrite("~p.\n", [Data]), [append]);
	_ -> lists:foldl(fun ee/2,Acc,R#xmlElement.content)
    end;
ee(#xmlText{parents=[{title,_},{channel,_},_], value=V},Acc) ->
    [channel,V|Acc]; % Channel name
ee(#xmlText{parents=[{title,_},{item,_},_,_], value=V},Acc) ->
    [title,V|Acc]; % News title
ee(#xmlText{parents=[{link,_},{item,_},_,_], value=V},Acc) ->
    [link,V|Acc]; % News link
ee(#xmlText{parents=[{description,_},{item,_},_,_], value=V},Acc) ->
    [description,V|Acc]; % Description
ee(#xmlText{parents=[{pubDate,_},{item,_},_,_], value=V},Acc) ->
    [pubDate,V|Acc]; % Publicity date
ee(#xmlText{}, Acc) -> Acc.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% reloader/0 - Runs loadXML every X minute to update RSS feed.
%% @end
%%--------------------------------------------------------------------

reloader(Pid) ->
    receive
	_ -> ok
    after 3000 ->
	    load(),
	    reloader(Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% closeProfile/1 - stops one specified inets process.
%% @end
%%--------------------------------------------------------------------

cP(Profile) ->
    inets:stop(httpc, Profile).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% terminate/0 - stops all inets processes.
%% @end
%%--------------------------------------------------------------------

terminate() ->
    inets:stop().



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% extractAll/0 - COPIED FUNCTION! Used to extract ALL elements from an xml file
%% @end
%%--------------------------------------------------------------------

ea() ->
    {ok,Path}=file:get_cwd(),
    {ok,Binary} = file:read_file([Path,"/rssFeed/cleanTest.xml"]),
    {Xml,_} = xmerl_scan:string(binary_to_list(Binary)),
    Data = [[{Element,Value} || #xmlElement{name=Element,content=Value1} <- XmlElem, #xmlText{value=Value} <- Value1] || #xmlElement{content=XmlElem} <- xmerl_xpath:string("//item", Xml)],
    file:write_file([Path, "/rssFeed/allXMLelements.txt"], io_lib:fwrite("~p\n", [Data])).
