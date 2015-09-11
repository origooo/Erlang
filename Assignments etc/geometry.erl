%%% @author Patrik Bäckström <origooo@dhcp-194212.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%% Module calculating the area and circumference of different shapes
%%% @end
%%% Created : 16 Sep 2013 by Patrik Bäckström <origooo@dhcp-194212.eduroam.chalmers.se>

-module(geometry).

-export([area/1, circumference/1, larger/2, sum_areas/1, largest/1]).

-define(PI, math:pi()).

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


%%--------------------------------------------------------------------
%% @doc
%% circumference/1 - Computes the cirumference of a shape
%% @end
%%--------------------------------------------------------------------
-spec circumference(Shape :: shape()) -> number().

circumference({circle, R})        -> 2 * R * ?PI;
circumference({square, X})        -> 4 * X;
circumference({rectangle, W , H}) -> 2 * W * H.


%%--------------------------------------------------------------------
%% @doc
%% larger/2 - Find which of the two shapes are bigger
%% @end
%%--------------------------------------------------------------------
-spec larger(Shape1 :: shape(), Shape2 :: shape()) -> shape().

larger(Shape1, Shape2) ->
    case area(Shape1) > area(Shape2) of
	true -> Shape1;
	false -> Shape2 %% Note: if same size, Shape2 is returned.
    end.


%%--------------------------------------------------------------------
%% @doc
%% sum_areas/1 - Sum the area of a list of shapes
%% @end
%%--------------------------------------------------------------------
-spec sum_areas(Shapes :: [shape()]) -> number().

sum_areas([])               -> 0;
sum_areas([Shape | Shapes]) -> area(Shape) + sum_areas(Shapes).


%%--------------------------------------------------------------------
%% @doc
%% largest/1 - Find the largest shape
%% @end
%%--------------------------------------------------------------------
-spec largest(Shapes :: [shape(), ...]) -> shape().

largest([Shape | Shapes]) -> largest(Shape, Shapes).


-spec largest(LargestSoFar :: shape(), Shapes :: [shape()]) -> shape().

largest(LargestSoFar, []) -> LargestSoFar;
largest(LargestSoFar, [Shape | Shapes]) ->
    largest(larger(LargestSoFar, Shape), Shapes).
