-module(seq).

-export([sum/1, sum_interval/2, create/1, create_reverse/1]).

sum(Num) when Num > 0 ->
    Num + sum(Num - 1);
sum(0) -> 0.

sum_interval(Interval, Num) when Interval =< Num ->
    Num + sum_interval(Interval, (Num - Interval));
sum_interval(_, _) -> 0.

create(Num) when Num > 0 ->
    [Num | create(Num - 1)];
create(0) -> [].

create_reverse(Num) ->
    create_reverse_helper(Num, []).

create_reverse_helper(0, Acc) ->
    Acc;
create_reverse_helper(Num, Acc) ->
    create_reverse_helper(Num-1, [Num|Acc]).
