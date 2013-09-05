%%% File

-module(seq_erlang).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).
-export([sum/1, mul/1, take_snd/1, fib/1,
         %% eff_fib/1,
         digitize/1, is_happy/1, all_happy/2,
         expr_parse/1, expr_print/1, expr_eval/1
         %%, expr_parse_2/1, expr_print_2/1, expr_eval_2/1
        ]).
%% sum
sum(_N) -> ok.

%% mul
mul(_N) -> ok.

%% take_snd
take_snd(_List) -> ok.

%% Fibonacci
fib(_N) -> ok.

%% Efficient fib.
%% eff_fib(_N) -> ok.

%% Digitify a number
digitize(_N) -> ok.

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
