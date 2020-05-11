(* Core Erlang concrete syntax generation 
 *
 *)

open Cerl

exception SyntaxError
exception InvalidArgumentError

let tab = "  "

let rec var_list_to_string = function
    [] -> ""
    | Var(v)::[] -> v ^ var_list_to_string []
    | Var(v)::l -> v ^ "," ^ var_list_to_string l
    | _ -> raise InvalidArgumentError

let rec values_to_string = function
    [] -> ""
    | Atom(a)::[] -> (Printf.sprintf "'%s'" a) ^ values_to_string []
    | Atom(a)::values -> (Printf.sprintf "'%s'" a) ^ "," ^ values_to_string values
    | Int(n)::[] -> (Printf.sprintf "%d" n) ^ values_to_string []
    | Int(n)::values -> (Printf.sprintf "%d" n) ^ "," ^ values_to_string values
    | Var(v)::[] -> v ^ values_to_string []
    | Var(v)::values -> v ^ "," ^ values_to_string values
    | _ -> raise InvalidArgumentError

let rec tuple_to_string = function
    [] -> ""
    | Atom(a)::[] -> (Printf.sprintf "'%s'" a) ^ values_to_string []
    | Atom(a)::values -> (Printf.sprintf "'%s'" a) ^ "," ^ values_to_string values
    | Int(n)::[] -> (Printf.sprintf "%d" n) ^ values_to_string []
    | Int(n)::values -> (Printf.sprintf "%d" n) ^ "," ^ values_to_string values
    | Var(v)::[] -> v ^ values_to_string []
    | Var(v)::values -> v ^ "," ^ values_to_string values
    | _ -> raise InvalidArgumentError

let rec gen_cerl (e : cexp) (tabs : string) : string =
    match e with
    | Apply(name, arity, args) ->
            Printf.sprintf "apply '%s'/%d(%s)"
                name
                arity
                (values_to_string args)

    | Atom(a) -> Printf.sprintf "'%s'" a
    | Int(n) -> Printf.sprintf "%d" n

    | Call(module_name, fun_name, args) ->
            Printf.sprintf "call '%s':'%s'(%s)"
                module_name
                fun_name
                (values_to_string args)

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
                (gen_cerl body (tabs ^ tab))

    | Fun(name, arity, vars, body) ->
            Printf.sprintf "'%s'/%d =\n%sfun (%s) ->\n%s%s\n"
                name
                arity
                tabs
                (var_list_to_string vars)
                (tabs ^ tab)
                (gen_cerl body (tabs ^ tab))

    | Let(vars, arg, body) ->
            Printf.sprintf "let %s =\n%s%s\n%sin\n%s%s"
                (gen_cerl vars tabs)
                (tabs ^ tab)
                (gen_cerl arg (tabs ^ tab))
                tabs
                (tabs ^ tab)
                (gen_cerl body (tabs ^ tab))

    | Module(name, exports, attributes, definitions) ->
            (*Printf.sprintf "module '%s' [%s] attributes [%s]\n%send"*)
            Printf.sprintf "module '%s'"
                name
                (*(gen_cerl exports tabs)
                (gen_cerl attributes tabs)
                (gen_cerl definitions tabs)*)
    | Export(name, arity) ->
            Printf.sprintf "'%s'/%d"
                name
                arity
    | Attribute(k, t) ->
            Printf.sprintf ""

    | Primop(name, args) ->
            Printf.sprintf "primop '%s'(%s)"
                name
                (List.fold_left (fun x y -> x ^ "," ^ y) ""
                    (List.map2 gen_cerl args (List.init (List.length args) (fun x -> tabs))))

    | Tuple(elements) ->
            Printf.sprintf "{%s}" (values_to_string elements)
    | Values(values) ->
            Printf.sprintf "<%s>" (values_to_string values)
    | Var(name) -> name

    | _ -> raise SyntaxError

let start_gen_cerl (e : cexp) : string =
    gen_cerl e tab
