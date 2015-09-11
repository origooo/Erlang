-module(partB).

-export([convert/1]).

f2c(Fahrenheit) ->
    (5*(Fahrenheit-32))/9.

c2f(Celsius) ->
    ((9*Celsius)/5)+32.

convert({c, X}) ->
    c2f(X);

convert({f, X}) ->
    f2c(X).
