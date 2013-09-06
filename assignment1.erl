%%% File

-module('assignment1').

%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).
-export([sum/1, mul/1, take_snd/1, fib/1,
         %% eff_fib/1,
         digitize/1, is_happy/1, all_happy/2,
         expr_parse/1, expr_print/1, expr_eval/1
         %%, expr_parse_2/1, expr_print_2/1, expr_eval_2/1
        ]).
%% sum
sum(N) when N > 0 ->
    N + sum(N - 1);
sum(0) ->
    0;
sum(_N) ->
    ok.

%% mul
mul(N) when N > 0 ->
    N * mul(N - 1);
mul(0) ->
    1;
mul(_N) -> ok.

%% take_snd
take_snd([H | T]) when H /= [] ->
    %[B || {_, B} <- List]; Assuming function head take_snd(List)
    %[X | take_snd(T); Assuming function head take_snd([{_, X} | T])
    [extract_element(H) | take_snd(T)];
take_snd(_List) -> 
    [].

extract_element({_A, B}) ->
    B.

%% Fibonacci
fib(Elements) ->
    fib_helper(Elements, 0, 1).
%fib(_N) -> ok.

fib_helper(Elements, Sum, Next) when Elements > 0 ->
    fib_helper(Elements - 1, Next, Sum + Next);
fib_helper(0, Sum, _Next) ->
    Sum.

%% Efficient fib.
%% eff_fib(_N) -> ok.

%% Digitify a number
digitize(N) when N >= 0 ->
    [Digit || Digit <- N];
digitize(_N) -> 0.

%% is_happy
is_happy(_N) -> ok.

%% all_happy
all_happy(_N, _M) -> ok.

%% Expressions

%% A. Evaluate expressions
expr_eval(_Expr) -> ok.

%% B. Pretty print expressions
expr_print(_Expr) -> ok.

%% C. Parse expressions
expr_parse(_Str) -> ok.

%% Advanced versions of expr
%% expr_eval_2(_Expr) -> ok.

%% expr_print_2(_Expr) -> ok.

%% expr_parse_2(_Str) -> ok.
