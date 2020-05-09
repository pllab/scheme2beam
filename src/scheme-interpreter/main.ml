
let primitive_env () : Env.env * Env.macro_table =
    (* Create an empty environment. *)
    let root_env = Env.make_env None in
    let macros = Hashtbl.create 10 in
    (* Load primitives implemented in OCaml. *)
    Primitives.load_primitives root_env;
    (* Load primitives implemented in Scheme. *)
    Primitives.load_scm_file
        root_env
        macros
        (Filename.concat (Filename.dirname Sys.executable_name)
        "src-scheme/primitives.scm");
    (root_env, macros)

let () =
  match Sys.argv with
  | [|_; "-h"|] | [|_; "--help"|] ->
     let usage = "usage: ./scheme [filename] [-h | --help]" in
     print_endline usage
  | [|_; path|] -> let (env, macros) = primitive_env () in
     Primitives.load_scm_file env macros path
  | _ ->
     prerr_endline "Invalid arguments provided.";
     exit 1
