
open Sexplib
open Sparse
open Gencerl

let load_scm_file env filepath =
  let sexps = Sexp.load_sexps filepath in
  let rec aux s = 
    match s with
    | [] -> ()
    | h::t -> let (env', expr') =  Sparse.parse env h in
              let mod = Filename.basename filepath in
              let core = Module(mod, Gencerl.get_func_descr expr', [], [expr'])
              let erl = Gencerl.start_gen_cerl core in 
              print_string erl
  in 
  (* (\* Change the current working directory.
   *  * Important for (load ...) when a relative path is given. *\)
   * let original_cwd = Sys.getcwd () in
   * Sys.chdir (Filename.dirname filepath); *)

  aux sexps
   
(*   (\* Restore the current working directory. *\)
 *   Sys.chdir original_cwd
 * (\* WARN: catch errors and close file properly. *\) *)

let primitive_env() : Cenv.env =
  (* Create an empty environment. *)
    let root_env = Cenv.make_env None in    
    (* (\* Load primitives implemented in OCaml. *\)
     * Primitives.load_primitives root_env; *)
    (* (\* Load primitives implemented in Scheme. *\)
     * load_scm_file
     *     root_env
     *     (Filename.concat (Filename.dirname Sys.executable_name)
     *     "src-scheme/primitives.scm"); *)
    root_env

let () =
  match Sys.argv with
  | [|_; "-h"|] | [|_; "--help"|] ->
     let usage = "usage: ./s2b [filename] [-h | --help]" in
     print_endline usage
  | [|_; path|] -> let env = primitive_env () in 
                   load_scm_file env path
  | _ ->
     prerr_endline "Invalid arguments provided.";
     exit 1
