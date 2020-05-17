
open Cerl
open Gencerl

module Ctx = Map.Make(String)
type environment = Cerl.cexp Ctx.t

type env = {
    parent : env option;  (* Parent frame of this environment. *)
    bindings : environment; (* map from cexps to cexps *)
}

let make_env parent_env =
    {
        parent = parent_env;
        bindings = Ctx.empty;
    }

let rec env_to_string env =
  let  {parent; bindings} = env in
  List.map (fun pr -> "key: " ^ (fst pr)) (Ctx.bindings bindings)

let rec lookup name env =
  let {parent; bindings} = env in
    try
        Ctx.find name bindings
    with
        Not_found -> match parent with  (* Lookup in parent frame. *)
                     | None -> failwith (name ^ " not found in " ^ (String.concat " " (env_to_string env)))
                     | Some parent_env -> lookup name parent_env

(* Add a value to an environment. *)
let add name ref_val env =
  let {parent; bindings} = env in
  let env' = Ctx.add name ref_val bindings (* Ctx.update name (fun _ -> Ctx.find_opt name bindings) bindings *)
  in {parent = parent;
      bindings = env';
     }

let rec set name value env =
    let {parent; bindings} = env in
    try
        Ctx.update name (fun _ -> Ctx.find_opt name bindings) bindings
    with
        Not_found -> match parent with  (* Lookup in parent frame. *)
                     | None -> failwith (name ^ " not found in environment. SET.")
                     | Some parent_env -> set name value parent_env
