-module (map_test).
-export ([start/0]).

% erlc map_test.erl
% erl -noshell -s map_test start -s init stop

% apparently the factorial function is resolved at runtime

start() ->
    L = lists:seq(1,4),
    M = lists:map(fun(X)->my_factorial:factorial(X) end, L),
    io:format("List is ~p~n", [M]).
    
