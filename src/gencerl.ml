(* Core Erlang concrete syntax generation 
 *
 *)

#use "cerl.ml"

exception SyntaxError
exception InvalidArgumentError

let tab = "  "

let rec var_list_to_string = function 
    [] -> ""
    | Var(v)::[] -> v ^ var_list_to_string []
    | Var(v)::l -> v ^ "," ^ var_list_to_string l
    | _ -> raise InvalidArgumentError

let print_values (values : cexp list) : string = 
    let rec values_to_string = function
    [] -> ""
    | Atom(a)::[] -> (Printf.sprintf "'%s'" a) ^ values_to_string []
    | Atom(a)::values -> (Printf.sprintf "'%s'" a) ^ "," ^ values_to_string values 
    | Int(n)::[] -> (Printf.sprintf "%d" n) ^ values_to_string []
    | Int(n)::values -> (Printf.sprintf "%d" n) ^ "," ^ values_to_string values
    | Var(v)::[] -> v ^ values_to_string []
    | Var(v)::values -> v ^ "," ^ values_to_string values 
    | _ -> raise InvalidArgumentError
    in Printf.sprintf "<%s>" (values_to_string values)

let rec gen_cerl (e : cexp) (tabs : string) : string =
    match e with
    | Atom(a) -> Printf.sprintf "'%s'" a
    | Int(n) -> Printf.sprintf "%d" n

    | Case(arg, clauses) ->
            Printf.sprintf "case %s of %s\n%send"
                (gen_cerl arg (tabs ^ tab)) 
                (List.fold_left (fun x y -> x ^ "\n" ^ y) "" 
                    (List.map2 gen_cerl clauses (List.init (List.length clauses) (fun x -> (tabs ^ tab)))))
                tabs
    | Clause(patterns, guard, body) -> 
            Printf.sprintf "%s%s when %s ->\n%s%s"
                tabs
                (gen_cerl patterns tabs)
                (gen_cerl guard tabs)
                (tabs ^ tab)
                (gen_cerl body tabs)

    | Fun(name, arity, vars, body) -> 
            Printf.sprintf "'%s'/%d =\n%sfun (%s) ->\n%s%s\n" 
                           name arity tabs (var_list_to_string vars) (tabs ^ tab) (gen_cerl body (tabs ^ tab)) 

    | Values(values) -> print_values values
    | Var(name) -> name

    | _ -> raise SyntaxError

let start_gen_cerl (e : cexp) : string =
    gen_cerl e tab
