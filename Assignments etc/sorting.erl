%%% @author Patrik Bäckström <origooo@dhcp-194212.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%% QuickSort
%%% @end
%%% Created : 16 Sep 2013 by Patrik Bäckström <origooo@dhcp-194212.eduroam.chalmers.se>

-module(sorting).

-export([qsort/1, qsort1/1, gen_qsort/2]).

qsort([]) -> [];
qsort([X]) -> [X];
qsort([Pivot | Xs]) ->
    Smaller = less_than_or_equal(Pivot, Xs),
    Larger = larger_than(Pivot, Xs),
    qsort(Smaller) ++ [Pivot] ++ qsort(Larger).

larger_than(_P, []) -> [];
larger_than(P, [H | T]) when H > P -> [H | larger_than(P, T)];
larger_than(P, [_H | T]) -> larger_than(P, T).

less_than_or_equal(_P, []) -> [];
less_than_or_equal(P, [H | T]) when H =< P -> [H | less_than_or_equal(P, T)];
less_than_or_equal(P, [_H | T]) -> less_than_or_equal(P, T).

qsort1([]) -> [];
qsort1([X]) -> [X];
qsort1([Pivot | Xs]) ->
    Smaller = [X || X <- Xs, X =< Pivot],
    Larger = [X || X <- Xs, X > Pivot],
    qsort1(Smaller) ++ [Pivot] ++ qsort(Larger).


-spec gen_qsort(CmpFun :: fun((any(), any()) -> boolean()), [any()]) -> [any()].
gen_qsort(_CmpFun, []) -> [];
gen_qsort(_CmpFun, [X]) -> [X];
gen_qsort(CmpFun, [Pivot | Xs]) ->
    gen_qsort(CmpFun, [X || X <- Xs, CmpFun(X, Pivot)])
	      ++ [Pivot]
	      ++ gen_qsort(CmpFun, [X || X <- Xs, not CmpFun(X, Pivot)]).
