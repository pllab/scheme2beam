(* Core Erlang concrete syntax generation 
 *
 *)

open Cerl
(*open Cenv*)

exception SyntaxError
exception InvalidArgumentError

let tab = "  "

(* todo this doesn't go through parent contexts yet *)
(*let func_names_from_binding (e: Cenv.env) : string list =
  List.map
  (fun pr -> fst pr ^ "/" ^ 
    (match (snd pr) with
     | Fun(nn,ar,l1,ex) -> (string_of_int ar))
   e.bindings)*)
    

let rec gen_cerl (e : cexp) (tabs : string) : string =
    match e with
    | Apply(name, arity, args) ->
            Printf.sprintf "apply '%s'/%d(%s)"
                name
                arity
                (String.concat ","
                    (List.map2 gen_cerl args
                        (List.init (List.length args) (fun x -> tabs))))

    | Call(module_name, fun_name, args) ->
            Printf.sprintf "call '%s':'%s'(%s)"
                module_name
                fun_name
                (String.concat ","
                    (List.map2 gen_cerl args
                        (List.init (List.length args) (fun x -> tabs))))

    | Case(arg, clauses) ->
            Printf.sprintf "case %s of %s\n%send"
                (gen_cerl arg (tabs ^ tab))
                (List.fold_left (fun x y -> x ^ "\n" ^ y) ""
                    (List.map2 gen_cerl clauses
                        (List.init (List.length clauses) (fun x -> (tabs ^ tab)))))
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
                (String.concat ","
                    (List.map2 gen_cerl vars
                        (List.init (List.length vars) (fun x -> tabs))))
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
            Printf.sprintf "module '%s' [%s]\n%sattributes [%s]\n%send\n"
                name
                (String.concat ","
                    (List.map2 gen_cerl exports
                        (List.init (List.length exports) (fun x -> tabs))))
                tabs
                (String.concat ","
                    (List.map2 gen_cerl attributes
                        (List.init (List.length attributes) (fun x -> tabs))))
                (String.concat "\n"
                    (List.map2 gen_cerl definitions
                        (List.init (List.length definitions) (fun x -> tabs))))
    | Export(name, arity) ->
            Printf.sprintf "'%s'/%d"
                name
                arity
    | Attribute(k, t) ->
            Printf.sprintf "%s = %s"
                (gen_cerl k tabs)
                (gen_cerl t tabs)

    | Primop(name, args) ->
            Printf.sprintf "primop '%s'(%s)"
                name
                (String.concat ","
                    (List.map2 gen_cerl args
                        (List.init (List.length args) (fun x -> tabs))))

    | Receive(clauses, timeout, action) ->
            Printf.sprintf "receive%s\n%safter %s ->\n%s%s"
                (List.fold_left (fun x y -> x ^ "\n" ^ y) ""
                    (List.map2 gen_cerl clauses
                        (List.init (List.length clauses) (fun x -> (tabs ^ tab)))))
                (tabs ^ tab)
                (gen_cerl timeout tabs)
                (tabs ^ tab ^ tab)
                (gen_cerl action (tabs ^ tab ^ tab))

    | Seq(exp1, exp2) ->
            Printf.sprintf "do\n%s%s\n%s%s"
                tabs
                (gen_cerl exp1 (tabs ^ tab))
                tabs
                (gen_cerl exp2 (tabs ^ tab))

    | Tuple(elements) ->
            Printf.sprintf "{%s}"
                (String.concat ","
                    (List.map2 gen_cerl elements
                        (List.init (List.length elements) (fun x -> tabs))))
    | Values(values) ->
            Printf.sprintf "<%s>"
                (String.concat ","
                    (List.map2 gen_cerl values
                        (List.init (List.length values) (fun x -> tabs))))
    | Var(name) -> name

    | Atom(a) -> Printf.sprintf "'%s'" a
    | Int(n) -> Printf.sprintf "%d" n

    | _ -> raise SyntaxError

let start_gen_cerl (e : cexp) : string =
    gen_cerl e tab
