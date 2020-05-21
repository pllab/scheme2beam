-module (send_recv).
-compile([export_all]).

serve() ->
    receive
        Request ->
	    et:trace_me(80,self(),run,[Request], []),
            io:format("Handling: ~s~n", [Request]),
            serve()
    end.


math() ->
    receive
        {add, X, Y} ->
            io:format("~p + ~p = ~p~n", [X,Y,X+Y]),
            math();
        {sub, X, Y} ->	    
            io:format("~p - ~p = ~p~n", [X,Y,X-Y]),
            math()
    end.

make_request(ServerId, Msg) ->
    et:trace_me(80,self(),ServerId,Msg,[]),
    ServerId ! Msg.

run() ->

    et_viewer:start([
		     {title,"Coffee Order"},
		     {trace_global,true},
		     {trace_pattern,{et,max}},
		     {max_actors,10}
		    ]),
    
    Pid = spawn(?MODULE, serve, []),
    make_request(Pid, request1),
    make_request(Pid, request2),

    timer:sleep(10),

    Pid2 = spawn(?MODULE, math, []),
    Pid2 ! {add, 1, 2},
    et:trace_me(80, self(), Pid2, {add, 1, 2}, []),
    Pid2 ! {sub, 3, 2},
    et:trace_me(80, self(), Pid2, {sub, 3, 2}, []),
    ok.
