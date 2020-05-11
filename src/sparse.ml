
open Cerl
open Cenv
open Sexplib

exception ParserError

(* for generating anonymous function names *) 
let next_l : int64 ref = ref Int64.zero
let fresh_l () : int64 = begin
    next_l := Int64.add (! next_l) Int64.one;
    ! next_l
    end

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let rec is_recursive(l: Sexp.t list) (name: string): bool =
  match l with
  | [] -> false
  | h::t -> let res = (match h with
    | Sexp.Atom(n) -> if n = name then true else false
    | Sexp.List(sub) -> is_recursive sub name)
    in if res = true then true else is_recursive t name

let rec parse(e: Cenv.env) (s: Sexp.t): (Cenv.env * Cerl.cexp) =
  match s with
  | Sexp.Atom(a) -> parse_atom e a
  | Sexp.List(Sexp.Atom("if") :: guard :: true_branch :: false_branch :: []) -> parse_cond e guard true_branch false_branch

  (* todo need to somehow check for recursive calls *)
  | Sexp.List(Sexp.Atom("let") :: arg_map :: body :: []) ->
     parse_let e arg_map body
     
  | Sexp.List(Sexp.Atom("define") :: params :: body :: []) ->
     parse_define e params body

  | Sexp.List(Sexp.Atom("lambda") :: arg_list :: body :: []) ->
     parse_func e "" arg_list body

  (* (\* at this point, just by default, we assume we're dealing with an application *\)
   * | Sexp.List(proc :: values) - >
   *     let n, arity = (match (parse e proc) with
   *       | environ, Fun(name, arity, args, body) -> name, arity
   *       | _ -> raise ParserError)
   *     in
   *     let e', value_list = parse e values
   *     in e', Apply(name, arity, Values(value_list)) *)
  | _ -> raise ParserError
and

  parse_define(e: env) (params: Sexp.t) (body: Sexp.t) : (env * cexp) = 
  let (name, args) =
    match params with
    | Sexp.Atom(n) -> (n, [])
    | Sexp.List(Sexp.Atom(h) :: []) -> (h, [])
    | Sexp.List(Sexp.Atom(h) :: t) -> (h, List.map (fun x -> let env', expr' = parse e x in expr') t)
  in
  let env', b = parse e body
  in
  (env', Fun(name, List.length args, args, b))
and
      
  parse_atom(e: env) (s : string) : (env * Cerl.cexp) =
  match s with
  | "#f" -> (e, Atom("false"))
  | "#t" -> (e, Atom("true"))
  | s when is_int s -> let i = (int_of_string s) in (e, Int(i))
  | s -> (e, Var(s))
  (* | s -> Atom(s) *) (* todo *)
and

parse_func(e: Cenv.env) (name: string) (args: Sexp.t) (body: Sexp.t) : (env * cexp) =
  let aux arglist = 
    match arglist with
    | Sexp.Atom(a) -> [Var(a)]
    (* assuming args are just strings and not evaluatable cerl expressions *)
    | Sexp.List(l) -> List.map (fun x -> match x with Sexp.Atom(v) -> Cerl.Var(v)) l
  in
  let e', b' = parse e body
  in
  let a = aux args
  in if name = ""
     then (e', Fun("_@c" ^ (Int64.to_string (fresh_l())),
                  List.length a,
                  a,
                  b'))
     else
       (e', Fun(name,
                  List.length a,
                  a,
                  b'))
and

parse_cond(e: Cenv.env) (guard: Sexp.t) (tb: Sexp.t) (fb: Sexp.t) : (env * cexp) = 
  let e', g = parse e guard in 
  let e'', t = parse e tb in
  let e''', f = parse e fb in 
  (e''', Case(Values([]),
           [ Clause(Values([]), g, t) ;
             Clause(Values([]), Atom("true"), f)
  ]))
and

parse_let(e: Cenv.env) (arg_map: Sexp.t) (body: Sexp.t) : (Cenv.env * cexp) = 
   let arg_list = parse_args e arg_map in
     let e', b' = parse e body in
     
     (* todo right now just pull out the first mapping *)
     let (a,e) = match arg_list with
       | [] -> (Values([]), Values([]))  (* todo also need to do this better *)
       | (expr, value)::t -> (expr, value) (* :: aux t *)
     in
     (e',
      Let(Values([a]),  (* todo assuming a is a Var... *)
          e,
          b'))
and

(* todo would this use the env if we used it at all? *)
parse_args(e: Cenv.env) (s: Sexp.t) : (Cerl.cexp * Cerl.cexp) list =
  match s with
  | Sexp.List([]) -> []

  (* the atom here has to be a var though... *)
  | Sexp.List(Sexp.List(x :: value :: []) :: []) ->
     let e', k = parse e x in
     let e'', v = parse e' value in
     [(k,v)]

  | Sexp.List(Sexp.List(x :: value :: []) :: t :: []) ->
     let e', k = parse e x in
     let e'', v = parse e' value in
     (k,v) :: parse_args e'' t
