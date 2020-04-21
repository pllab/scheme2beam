-module(factorial).

fact(0) -> 1;
fact(N) -> N * fact(N-1).
