
open Cerl
open Cenv

let milnerize (e: env) (expr: cexp) : (env * cexp) =       
  match expr with
  | Atom(a) | Int(a) | Var(a) -> let chan = spawn(module?, fun x -> ???, []) in chan ! a  (* closed term *)

  (* based on yesterday's conversation, unsure if this is a let/define;
     I suspect we should only focus on strict lambda expressions, luckily we have some
     church encodings in examples/scheme 
   *)		     
  | Let(args, assignment, body) -> (* abstraction *)
  | Apply(name, arity, value_list) -> (* application *)
	 
       
