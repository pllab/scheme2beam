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

    let p = gen_cerl (Fun("foo",0,[],Atom("ok"))) in
    print_string "Test fun with 0 arg:\t\t";
    print_endline(test_to_string (p = "'foo'/0 = fun () -> 'ok'\n"));

    let p = gen_cerl (Fun("id",1,[Var("I")],Var("I"))) in
    print_string "Test fun with 1 arg:\t\t";
    print_endline(test_to_string (p = "'id'/1 = fun (I) -> I\n"));

    (* case and clause test *)
    (*
    let p = gen_cerl (Fun("a",
                          1,
                          [Var("N")],
                          Case(Var("I"), [Clause(), Clause()]))) in
    print_string "Test case and clause:\t\t";
    print_endline(test_to_string (p = "'id'/1 = fun (I) -> I\n"));
    print_string p
    *)
