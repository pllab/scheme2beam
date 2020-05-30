open Ir
open Gencerl

let () = 
    let e, m = milnerize (Cenv.make_env None) (Fun("foo",1,[Var("X")],Var("X")))
    in
    print_endline "Milnerize identity function:";
    print_endline (start_gen_cerl m);

