% hello world program
% erlc helloworld.erl
% erl -noshell -s helloworld start -s init stop
-module(helloworld).
-export([start/0]).

start() ->
io:fwrite("Hello World!\n").
