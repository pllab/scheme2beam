
-module(anon).
-export([start/0]).

start() ->
    Sum = fun(A, B) -> A + B end,
    Sum(4, 3).
