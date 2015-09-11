%%% @author Patrik Bäckström <origooo@dhcp-194130.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2013 by Patrik Bäckström <origooo@dhcp-194130.eduroam.chalmers.se>

-module(hotload).

-export([server/1, upgrade/1]).
 
server(State) ->
    receive
	update ->
	    NewState = ?MODULE:upgrade(State),
	    ?MODULE:server(NewState);  %% loop in the new version of the module
	SomeMessage ->
	    %% do something here
	    server(State)  %% stay in the same version no matter what.
    end.
 
upgrade(OldState) ->
    %% transform and return the state here.
