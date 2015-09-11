%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-204149.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%% Cool functions änna...
%%% @end
%%% Created : 14 Oct 2013 by Patrik Bäckström <origooo@dhcp-204149.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(distgemoetry).

-define(PI, math:pi()).
-define(AREA_SERVER,area_srv).

-export([start/0,area/1,loop/0,area_rpc/2]).

-type circle()       :: {circle, number()}.
-type square()       :: {square, number()}.
-type rectangle()    :: {rectangle, number(), number()}.

-type shape() :: circle() | square() | rectangle().


%%--------------------------------------------------------------------
%% @doc
%% area/1 / Computes the area of a shape
%% @end
%%--------------------------------------------------------------------
-spec area(Shape :: shape()) -> number().

area({circle, N})       -> N * N * ?PI;
area({square, X})       -> X * X;
area({rectangle, W, H}) -> W * H.


loop() ->
    receive
	{{Sender,Ref},Shape} ->
	    Area=area(Shape),
	    Sender ! {area_result,Ref,Area},
	    loop()
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

area_rpc(Server,Shape) ->
    Ref=make_ref(),
    ?AREA_SERVER!{{self(),Ref},Shape},
    receive
	{area_result,Ref,Result} ->
	    Result
	after 100 ->
		{err_err,no_result}
    end.

start() ->
    register(?AREA_SERVER,spawn(fun distgemoetry:loop/0)).
