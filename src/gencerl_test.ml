(* Core Erlang concrete syntax generation tests
 *
 *)

#use "gencerl.ml"

let test_to_string a =
    match a with
    | true -> "[test passed]"
    | false -> "[TEST FAILED]"

let () =
    print_endline("\n(*----------*)\n CORE ERLANG GEN TEST \n(*----------*)\n");

    let p = start_gen_cerl (Fun("foo",0,[],Atom("ok")))
    in
    print_endline "Test fun with 0 arg:";
    print_endline p;

    let p = start_gen_cerl (Fun("id",1,[Var("I")],Var("I")))
    in
    print_endline "Test fun with 1 arg:";
    print_endline p;

    let p = start_gen_cerl (Fun("b",
                          1,
                          [Var("N")],
                          Case(Var("N"),
                            [Clause(Values([Int(42)]), Atom("true"), Atom("ok"));
                             Clause(Values([Var("_@b0")]), Atom("true"), Atom("error"))])))
    in
    print_endline "Test case and clauses:";
    print_endline p;

    let p = start_gen_cerl (Fun("c",
                            3,
                            [Var("_@c2"); Var("_@c1"); Var("_@c0")],
                            Case(Values([Var("_@c2"); Var("_@c1"); Var("_@c0")]),
                                [Clause(Values([Atom("left"); Var("L"); Var("R")]), Atom("true"), Var("L"));
                                 Clause(Values([Atom("right"); Var("L"); Var("R")]), Atom("true"), Var("R"))])))
    in
    print_endline "Test case and clauses with value list:";
    print_endline p;

    let p = start_gen_cerl (Fun("d",
                            2,
                            [Var("_@d1"); Var("_@d0")],
                            Case(Values([]),
                                [Clause(Values([]), Call("erlang", ">", [Var("_@d1"); Var("_@d0")]), Atom("greater"));
                                 Clause(Values([]), Atom("true"), Atom("not_greater"))])))
    in
    print_endline "Test if-then-else styled with case and clauses:";
    print_endline p;

    let p = start_gen_cerl (Fun("d",
                            1,
                            [Var("_@d0")],
                            Case(Values([]),
                                [Clause(Values([]), Call("erlang", ">", [Var("_@d0"); Int(1)]), Atom("greater"));
                                 Clause(Values([]), Atom("true"), Atom("not_greater"))])))
    in
    print_endline "Test if-then-else styled with case and clauses:";
    print_endline p;

