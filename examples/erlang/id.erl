-module (id).
-compile([export_all]).

id_recv() ->
	  receive
		X ->
		  io:format("~p", [X])
	end.

id() ->
  receive {X, R} ->
    R ! X, 
    id()
  end.

run() ->
     A = spawn(?MODULE, id, []),
     B = spawn(?MODULE, id_recv, []), 

     A !  {"hey", B},
    ok.
