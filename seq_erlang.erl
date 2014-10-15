%%% File

-module(seq_erlang).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).
-export([sum/1, mul/1, take_snd/1, fib/1,
         %%eff_fib/1,
         digitize/1, is_happy/1, all_happy/2,
         expr_parse/1, expr_print/1, expr_eval/1
         %%, expr_parse_2/1, expr_print_2/1, expr_eval_2/1
        ]).
%% sum
sum(0) -> 0;
sum(N) when N > 0 -> N + sum(N - 1).

%% mul
mul(0) -> 1;
mul(N) -> N * mul(N - 1).

%% take_snd
take_snd([{_, B} | T]) -> [B | take_snd(T)];
take_snd([]) -> [].

%% Fibonacci
%% I presume this is the effective implementation.
%% I would start with 0 and 1 instead of 1, 1 though. But for the sake of testing I changed it to 1, 1.
fib(Elements) -> fib_helper(Elements, 1, 1).

fib_helper(0, Sum, _Next) -> Sum;
fib_helper(Elements, Sum, Next) -> fib_helper(Elements - 1, Next, Sum + Next).

%% Digitify a number
digitize(N) -> digitize_helper(N, []).

digitize_helper(N, Acc) when N > 0 -> digitize_helper(N div 10, [N rem 10 | Acc]);
digitize_helper(0, Acc) -> Acc.

%% is_happy
is_happy(0) -> false;
is_happy(1) -> true;
is_happy(4) -> false;
is_happy(N) -> is_happy(sum_elements(digitize(N))).

sum_elements([]) -> 0;
sum_elements([H | T]) -> H * H + sum_elements(T).


%% all_happy
all_happy(N, M) when N =< M ->
    case is_happy(N) of
	true  -> [N | all_happy(N + 1, M)];
	false -> all_happy(N + 1, M)
    end;
all_happy(_N, _M) -> [].

%% Expressions

%% A. Evaluate expressions
expr_eval({Indicator, N, M}) ->
    case Indicator of
	mul   -> expr_eval(N) * expr_eval(M);
	plus  -> expr_eval(N) + expr_eval(M);
	minus -> expr_eval(N) - expr_eval(M)
    end;
expr_eval({num, Number}) ->
    Number.

%% B. Pretty print expressions
expr_print(Expr) ->
    expr_print2(Expr).
expr_print2({mul, N, M})   -> lists:concat(["(", expr_print2(N), "*", expr_print2(M), ")"]);
expr_print2({plus, N, M})  -> lists:concat(["(", expr_print2(N), "+", expr_print2(M), ")"]);
expr_print2({minus, N, M}) -> lists:concat(["(", expr_print2(N), "-", expr_print2(M), ")"]);
expr_print2({num, N})      -> integer_to_list(N).



%% C. Parse expressions
expr_parse(Expr)                                        -> parse(Expr,[],0).
parse([H=$(|T],Acc,Level)                               -> NewLevel=Level+1,
							   parse(T,Acc++[H],NewLevel);
parse([H=$)|T],Acc,Level)                               -> NewLevel=Level-1,
							   parse(T,Acc++[H],NewLevel);
parse([H|T],[_AccH|AccT],Level) when H>41,H<46,Level=<1 -> {operator(H),parse(AccT,[],0),parse(T,[],0)};
parse([H|T],_,Level) when H>47,H<58,Level==0            -> findDigits([H|T],0);
parse([H|T],Acc,Level)                                  -> parse(T,Acc++[H],Level);
parse([],_,Level)                                       -> Level.

findDigits([H|T],Sum) when H/=[], H/=$) -> findDigits(T,Sum*10+H-48);
findDigits(_T,Sum)                     -> {num,Sum}.

operator($+) -> plus;
operator($-) -> minus;
operator($*) -> mul.

%% Advanced versions of expr
%% expr_eval_2(_Expr) -> ok.

%% expr_print_2(_Expr) -> ok.

%% expr_parse_2(_Str) -> ok.
