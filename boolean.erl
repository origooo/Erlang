-module(boolean).

-export([b_not/1, b_and/2, b_or/2]).

b_not(Bool) ->
    case Bool of
	    true -> false;
	    false -> true
    end.

b_and(Bool1, Bool2) ->
    case {Bool1, Bool2} of
	{true, true} -> true;
	{true, false} -> false;
	{false, true} -> false;
	{false, false} -> false
    end.

b_or(Bool1, Bool2)->
    case {Bool1, Bool2} of
	{true, true} -> true;
	{true, false} -> true;
	{false, true} -> true;
	{false, false} -> false
    end.

