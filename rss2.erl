%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-203187.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2013 by Patrik Bäckström <origooo@dhcp-203187.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(rss2).
-include_lib("xmerl/include/xmerl.hrl").

-export([fetch/0,ee/2]).

-define(XML_RE, "[^<]+|<(?:!(?:--(?:[^-]*-(?:[^-][^-]*-)*->?)?"
                "|\\[CDATA\\[(?:[^\\]]*](?:[^\\]]+])*]+"
                "(?:[^\\]>][^\\]]*](?:[^\\]]+])*]+)*>)?"
                "|DOCTYPE(?:[ \\n\\t\\r]+(?:[A-Za-z_:]"
                "|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*(?:[ \\n\\t\\r]+(?:(?:[A-Za-z_:]"
                "|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*|\"[^\"]*\""
                "|'[^']*'))*(?:[ \\n\\t\\r]+)?"
                "(?:\\[(?:<(?:!(?:--[^-]*-(?:[^-][^-]*-)*->"
                "|[^-](?:[^\\]\"'><]+|\"[^\"]*\"|'[^']*')*>)"
                "|\\?(?:[A-Za-z_:]|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*(?:\\?>"
                "|[\\n\\r\\t ][^?]*\\?+(?:[^>?][^?]*\\?+)*>))"
                "|%(?:[A-Za-z_:]|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*;|[ \\n\\t\\r]+)*](?:[
 \\n\\t\\r]+)?)?>?)?)?"
                "|\\?(?:(?:[A-Za-z_:]|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*(?:\\?>"
                "|[\\n\\r\\t ][^?]*\\?+(?:[^>?][^?]*\\?+)*>)?)?"
                "|/(?:(?:[A-Za-z_:]|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*(?:[ \\n\\t\\r]+)?>?)?|(?:(?:[A-Za-z_:]"
                "|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*(?:[ \\n\\t\\r]+(?:[A-Za-z_:]"
                "|[^\\x00-\\x7F])(?:[A-Za-z0-9_:.-]"
                "|[^\\x00-\\x7F])*(?:[ \\n\\t\\r]+)?="
                "(?:[ \\n\\t\\r]+)?(?:\"[^<\"]*\""
                "|'[^<']*'))*(?:[ \\n\\t\\r]+)?/?>?)?)").


fetch() ->
    {ok,{_,_,RSS}}=httpc:request(get, {"http://articlefeeds.nasdaq.com/nasdaq/categories?category=Economy",[]},[],[]),
    TidyString=replace_char_ref(RSS),
    {XML,_}=xmerl_scan:string(TidyString),
    ee(XML,[]).

replace_char_ref(String) ->
    Ampersand=re:replace(String,"&amp;","\\&",[{return,list},global]),
    Ampersand2=re:replace(Ampersand,"&amp;","\\&",[{return,list},global]),
    LessThan=re:replace(Ampersand2,"&lt;","<",[{return,list},global]),
    GreaterThan=re:replace(LessThan,"/&gt;","/>",[{return,list},global]),
    Apostrophe=re:replace(GreaterThan,"&#39;","'",[{return,list},global]),
    ReplaceAmp=re:replace(Apostrophe,"&"," and ",[{return,list},global]),
    ReplaceAmp.


ee(R,Acc) when is_record(R,xmlElement) ->
    %{ok,Path}=file:get_cwd(),

    case R#xmlElement.name of
        item ->
            ItemData=lists:foldl(fun ee/2,[],R#xmlElement.content),
            Data=[ItemData|Acc],
	    send_to_DB(Data);
	    %file:write_file([Path, "/rss2.txt"], io_lib:fwrite("~p.\n", [Data]), [append]);
	_ -> lists:foldl(fun ee/2,Acc,R#xmlElement.content)
    end;
ee(#xmlText{parents=[{title,6},{item,_},{channel,2},{rss,3}], value=V},Acc) ->
    [{title,V}|Acc];
ee(#xmlText{parents=[{link,4},{item,_},{channel,2},{rss,3}], value=V},Acc) ->
    [{link,V}|Acc];
ee(#xmlText{parents=[{description,8},{item,_},_,_], value=V},Acc) ->
    [{description,V}|Acc];
ee(#xmlText{}, Acc) -> Acc.

send_to_DB([]) -> ok;
send_to_DB([H|T]) ->
    to_JSON(H),
    %send(),
    send_to_DB(T).

to_JSON([H|T]) ->
    case H of
	{Header,Value} -> %when is_list(Value), is_atom(Header) ->
	   "{\"", Header, "\" : ",Value,"}"
    end.
	    
