
open Printf
open Sexplib

open Cenv
open Cerl
open Gencerl
open Ir
open Sparse

(* todo this doesn't go through parent contexts yet *)
let func_names_from_binding (e: Cenv.env) : cexp list =
  let env_bindings = e.bindings in
  let map_bindings = Ctx.bindings env_bindings in
  List.map
  (fun pr -> 
    (match (snd pr) with
     | Fun(nn,ar,l1,ex) -> Export((fst pr), ar)))
   map_bindings


let load_scm_file ~environment:env ~fpath:filepath ?outfile:(out="") =
  let sexps = Sexp.load_sexps filepath in

  (* returns list of strings, each string is a function *)
  (* this operates on a "per function level", the sexp that gets read in will, 
     by tradition in scheme, just be a lambda, or a function;
     the sexps are in a list, so we process the head, then pass the new environment to a call
     that processes the tail, recursively, again keeping track of the possibly modified 
     environment, which is how we read out the function names for the module boilerplate below.
   *)
  let rec aux s env_local = 
    match s with
    | [] -> ([], env_local)
    | h::t -> let (env', expr') = (Sparse.parse env_local h)
	      in let (tail, env'') = aux t env'
		 in (expr' :: tail, env'')
  in 
  let (instrs, env_final) = aux sexps env
  in


  (* IR pass*)
  (* we might need access to functions outside the one we were parsing above, *)
  (* so we should probably just do a second pass over the cexps here for the IR pass ;
     this should result in another list of cexps, only with a bunch of send/recvs;
     unsure how the environment plays into all this, but we will probably need
     to have a new one, or store some stuff in it
   *)
  (* milnerize "func" is a Definition *)
  let milnerized = List.map (fun func -> snd (Ir.milnerize env_final func)) instrs
  in

  (* (\* final main routine to call everything *\) *)
  (* [Fun("start", 0,  ; milnerized] *)
  
  (* module boilerplate *)
  let erlmod = Filename.basename filepath in
  let prog = [Module(erlmod, func_names_from_binding env_final, [], milnerized)]
  in
  
  (* if out != "" *)
  (* then dump2file prog out *)
  (* else *)
    let outstr = List.fold_right (fun line acc -> acc ^ "\n" ^ (start_gen_cerl line)) prog "" in
    print_string outstr
   

let primitive_env() : Cenv.env =
  (* Create an empty environment. *)
    let root_env = Cenv.make_env None in    
    root_env

let () =
  match Sys.argv with
  | [|_; "-h"|] | [|_; "--help"|] ->
     let usage = "usage: ./s2b [filename] [-h | --help | -o outfilename]" in
     print_endline usage
  | [|_; path|] -> let env = primitive_env () in 
		   load_scm_file ~environment:env ~fpath:path ~outfile:""
  | [|_; path; out|] -> let env = primitive_env () in
			 load_scm_file ~environment:env ~fpath:path ~outfile:out
  | _ ->
     prerr_endline "Invalid arguments provided.";
     exit 1
