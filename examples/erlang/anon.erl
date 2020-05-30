-module (anon).

id() ->
	I = fun (X) -> X end,
	I(2345).
