

-module(anon2).
-export([start/0]).

start() ->
    fun(A, B) -> A end.
