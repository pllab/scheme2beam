-module(tracer).

-compile(export_all).

-record(state, {stacks=[]}).

test_tracer() ->
    Tracer = spawn_tracer(),
    erlang:trace_pattern({?MODULE, '_', '_'}, [], [local]),
    erlang:trace(self(), true, [call,
                                arity,
                                return_to,
                                %% procs,
                                %% running,
                                timestamp,
                                {tracer, Tracer}]),
    loop(100),
    Tracer!dump.

spawn_tracer() ->
    spawn(fun()-> trace_listener(#state{}) end).

trace_listener(State) ->
    receive
        dump ->
            io:format("~p", [lists:reverse(State#state.stacks)]);
        {trace_ts, Pid, call, MFA, Ts} ->
            Stacks = State#state.stacks,
            trace_listener(State#state{stacks=[MFA|Stacks]});
        _Term ->
            io:format("~p~n", [_Term]),
            trace_listener(State)
    end.

loop(0) -> ok;
loop(N) -> loop(N-1).

%% loop(10),
%% timer:sleep(100),
%% Tracer ! dump.
