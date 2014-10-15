%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2013 by Patrik Bäckström <origooo@dhcp-196060.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(stockSymbols).

-export([fetchNDAXsymbols/0]).

fetchNDAXsymbols() ->
    Path=file:get_cwd(),
    %{ok,Result}=
    file:open([Path,"/test.csv"],[read]).
    %file:read_line(IoDevice,[]).

%'http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download';
