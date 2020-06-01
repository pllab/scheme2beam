
open Cerl
open Cenv

exception NonMilnerizableError

(* for generating anonymous function names *) 
let next_l : int64 ref = ref Int64.zero
let fresh_l () : int64 = begin
    next_l := Int64.add (! next_l) Int64.one;
    ! next_l
    end

let rec subst (x : string) (a : cexp) : cexp =
    let () = print_endline x in
    match a with
    | Var(v) -> Var(x)
    | Apply(f,n,a) -> Apply(f,n,a)
    | Let(f,n,a) -> Let(f,n,a)
    | _ -> raise NonMilnerizableError

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
  | Fun(name, _, args, body) ->
          (* [[λxM]]a := νa !a(xr).[[M]]br_b | [] *)
          let () = print_endline name in
          let x = match args with
                  | [] -> Var("X")
                  | Var(v) :: rest_args -> Var("X_" ^ v ^ (Int64.to_string (fresh_l())))
          in
          let m = match x with
                  | Var(x_name) -> subst x_name body 
                  | _ -> raise NonMilnerizableError
          in
          let e', body' = match body with
            | Var(_) -> 
                e, Receive(
                  [Clause(Values([Tuple([x; Var("R")])]),
                    Atom("true"),
                    Seq(Call("erlang", "!", [Var("R"); m]),
                        Apply(name, 0, []))
                  )], 
                  Atom("infinity"), Atom("true"))
            | Apply(_,_,_) -> milnerize e body
            | _ -> raise NonMilnerizableError
          in
          (e', Fun(name, 0, [], body'))
	    
  | Apply(fname, _, args) ->
          (* [[MN]]a := νr [[M]]b[[N]]cb_cr | r(a).[] *)
          (* in core erlang:
           * let <Lhs> = spawn fname in
           * let <Recv> = fun () -> receive X -> X end in 
           * let Rhs = spawn Recv in 
           * call ! (Lhs, Tuple(arg, Rhs))
           *)
          let arg = match args with
                    | h::t -> h
                    | [] -> raise NonMilnerizableError (* can't apply with empty argument *)
          in
          let lhs_var = "Lhs_" ^ fname
          in
          let recv_var = "_recv@" ^ (Int64.to_string (fresh_l()))
          in
          let rhs_var = "Rhs_" ^ fname
          in
          let rhs = 
              Let(
                  Values([Var(rhs_var)]),
                  Call("erlang", "spawn", [Var(recv_var)]),
                  Call("erlang", "!", [Var(lhs_var); Tuple([arg; Var(rhs_var)])]))
          in 
          let recv = 
              Let(
                  Values([Var(recv_var)]),
                  Fun("", 0, [],
                    Receive(
                      [Clause(Values([Var("X")]),
                        Atom("true"),
                        Var("X")
                      )], 
                      Atom("infinity"), Atom("true"))),
                  rhs)
          in 
          let lhs = 
              Let(
                  Values([Var(lhs_var)]),
                  Call("erlang", "spawn", [Atom(fname)]),
                  recv)
          in
          e, lhs
	       
  | Export(name, _) -> e, Export(name, 0)
  | Definition(exp, func) -> 
          let env', mil_func = milnerize e func 
          in
          let env'', mil_exp = milnerize env' exp
          in 
          env'', Definition(mil_exp, mil_func)
     
  | _ -> raise NonMilnerizableError
(*
  | Let(args, assignment, body) -> (* abstraction *)
*)
	 
       
