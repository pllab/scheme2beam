% to emit core erlang:
% $ erlc +to_core core_example.erl
-module(casebind).
%-export([casebind/1]).
i(E) ->
    case E of
        a ->
            X = 1,
            Y = 10;
        b ->
            X = 23,
            Y = 17
    end,
    {X,Y}.
