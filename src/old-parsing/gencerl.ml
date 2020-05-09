(* Core Erlang concrete syntax generation 
 *
 *)

#use "cerl.ml"

exception SyntaxError
exception InvalidArgumentError

let rec var_list_to_string = function 
[] -> ""
| Var(v)::[] -> v ^ var_list_to_string []
| Var(v)::l -> v ^ "," ^ var_list_to_string l
| _ -> raise InvalidArgumentError

let rec gen_cerl (e : cexp) : string =
    match e with
    | Atom(a) -> Printf.sprintf "'%s'" a
    | Int(n) -> Printf.sprintf "%d" n

    (*
    | Case(arg, clauses) ->
    | Case(patterns, body) -> 
    *)

    | Fun(name, arity, vars, body) -> 
            Printf.sprintf "'%s'/%d = fun (%s) -> %s\n" 
                           name arity (var_list_to_string vars) (gen_cerl body) 

    | Var(name) -> name

    | _ -> raise SyntaxError
