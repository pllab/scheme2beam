
open Cerl
open Cenv

exception NonMilnerizableError

let rec milnerize (e: env) (expr: cexp) : (env * cexp) =       
  match expr with
  (*
  | Atom(a) | Int(a) | Var(a) -> let chan = spawn(module?, fun x -> ???, []) in chan ! a  (* closed term *)
  *)

  (* based on yesterday's conversation, unsure if this is a let/define;
     I suspect we should only focus on strict lambda expressions, luckily we have some
     church encodings in examples/scheme 
   *)		     
  | Var(v) -> e, Var(v)
  | Fun(name, arity, args, body) ->
          (* [[λxM]]a := νa !a(xr).[[M]]brb | [] *)
          let x = match args with
                  | [] -> Var("X")
                  | Var(v) :: rest_args -> Var(v)
          in
          let e', m = milnerize e body (* this should be the "eval" *)
          in
          let recv = 
              Receive(
                  [Clause(Values([x; Var("R")]),
                    Atom("true"),
                    Seq(Call("erlang", "!", [Var("R"); m]), 
                        Apply(name, arity, []))
                  )], 
                  (* Receive boilerplate for timeout *)
                  Atom("infinity"), Atom("true"))
          in
          (e', Fun(name, arity, [], recv))
   | _ -> raise NonMilnerizableError
(*
  | Let(args, assignment, body) -> (* abstraction *)
  | Apply(name, arity, value_list) -> (* application *)
*)
	 
       
