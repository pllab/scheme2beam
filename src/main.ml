
open Printf
open Sexplib

open Cenv
open Cerl
open Gencerl
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
  let rec aux s = 
    match s with
    | [] -> []
    | h::t -> let (env', expr') =  Sparse.parse env h in
              let erlmod = Filename.basename filepath in
              [Module(erlmod, func_names_from_binding env', [], [expr'])]
  in 
  let instr = aux sexps
  in 
  if out != ""
  then dump2file instr out
  else let outstr = List.fold_right (fun line acc -> acc ^ "\n" ^ (start_gen_cerl line)) instr "" in
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
