-module (id2).
-compile([export_all]).

id() ->
  receive {X, R} ->
    R ! X, 
    id()
  end.

run() ->
    A = spawn(?MODULE, id, []),
    B = spawn(fun () -> receive (X) -> io:format("~p", [X]) end end), 

    A ! {"hey", B}.

