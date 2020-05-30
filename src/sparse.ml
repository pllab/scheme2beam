
open Cerl
open Cenv
open Gencerl
open Sexplib

exception ParserError of string

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
  | Sexp.Atom(a) ->
     let e', expr' = parse_atom e a in e', expr'
  | Sexp.List(Sexp.Atom("if") :: guard :: true_branch :: false_branch :: []) ->
     let e', expr' = parse_cond e guard true_branch false_branch in e', expr'

  | Sexp.List(Sexp.Atom("let") :: arg_map :: body :: []) ->
     let e', expr' = parse_let e arg_map body in e', expr'
     
  | Sexp.List(Sexp.Atom("define") :: params :: body :: []) ->
     let e', expr' = parse_define e params body in
     e', expr'

  | Sexp.List(Sexp.Atom("lambda") :: arg_list :: body :: []) ->
     let e', expr' = parse_func e "" arg_list body in e', expr'

  | Sexp.List(Sexp.Atom(op) :: arg1 :: arg2 :: []) ->
     let e', expr' = parse_binop e op arg1 arg2 in e', expr'

  (* at this point, just by default, we assume we're dealing with an application *)
  | Sexp.List(proc :: values) ->
     let e', n, arity =
       (match (parse e proc) with
        | environ, Var(fname) ->     (* parsed into a variable, e.g. "Var('factorial')" so we look it up *)
           begin match (Cenv.lookup fname e) with
           | Fun(name, ar, args, b) -> environ, name, ar
           | _ -> raise (ParserError ("var: " ^ fname ^ " is not a function object"))
           end
        | environ, Fun(name, arity, args, body) -> environ, name, arity   (* parsed into a lambda *)
        | _ -> raise (ParserError ("Got stuck on: " ^ Sexp.to_string s)))
     in
     
     (* todo each one of these could change the env... 
	replace map with just a match recursion to thread each new env through 
      *)
      let value_list = List.map (fun x -> let env', expr' = parse e' x in expr') values
      in e', Apply(n, arity, value_list)
  | _ -> raise (ParserError ("Global stuck: " ^ Sexp.to_string s ))
and

  parse_define(e: env) (params: Sexp.t) (body: Sexp.t) : (env * cexp) = 
  let (name, args) =
    match params with
    | Sexp.Atom(n) -> (n, [])
    | Sexp.List(Sexp.Atom(h) :: []) -> (h, [])
    | Sexp.List(Sexp.Atom(h) :: t) -> (h, List.map (fun x -> let env', expr' = parse e x in expr') t)
  in
  (* add func here with empty body to avoid recursion problem *)
  let env' = Cenv.add name (Fun(name, List.length args, args, Values([]))) e  
  in
  let env'', b = parse env' body
  in
  (* update here with body filled in *)
  let env''' = Cenv.add name (Fun(name, List.length args, args, b)) env''
  in
  (env''', Definition(Export(name, List.length args),Fun(name, List.length args, args, b)))
and
      
  parse_atom(e: env) (s : string) : (env * Cerl.cexp) =
  match s with
  | "#f" -> (e, Atom("false"))
  | "#t" -> (e, Atom("true"))
  | s when is_int s -> let i = (int_of_string s) in (e, Int(i))
  | s -> (e, Var(s))
  (* | s -> Atom(s) *) (* todo *)
and

  parse_binop(e: env) (op: string) (arg1: Sexp.t) (arg2: Sexp.t) : (env * Cerl.cexp) =
  match op with
  | "<=" | "*"  | "+"  | "-"  | "<"  | ">"  | "=>" | "/" ->
     let e', a1 = (parse e arg1) in 
     let e'', a2 = (parse e' arg2) in
     (e, Call("erlang", op,  [a1 ; a2]))
  | _ -> raise (ParserError ("Binop exn: " ^ op ^ " " ^ Sexp.to_string arg1 ^ " " ^ Sexp.to_string arg2 ))
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
     then 
     let anon_name ="_@" ^ (Int64.to_string (fresh_l())) 
     in (e', Fun(anon_name,
                 List.length a,
                 a,
                 b'))
     (* think we should break lambda functions in core erlang Lets, 
      * like let <_@anon> = fun (LAMBDA ARGS) -> LAMBDA BODY in APPLICATION of LAMBDA.
      * But not sure how to pass in the body for the Let here. *)
     (*in (e', Let(Values([Var(anon_name)]),
                   Fun(anon_name,
                       List.length a,
                       a,
                       b'), )*)
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
     
     (* todo right now we just pull out the first mapping *)
     let (a,e) = match arg_list with
       | [] -> (Values([]), Values([]))
       | (expr, value)::t -> (expr, value) (* :: aux t *)
     in
     (e',
      (* todo assuming a is a Var *)
      Let(Values([a]),  
          e,
          b'))
and


parse_args(e: Cenv.env) (s: Sexp.t) : (Cerl.cexp * Cerl.cexp) list =
  match s with
  | Sexp.List([]) -> []

  (* the atom here has to be a var though *)
  | Sexp.List(Sexp.List(x :: value :: []) :: []) ->
     let e', k = parse e x in
     let e'', v = parse e' value in
     [(k,v)]

  | Sexp.List(Sexp.List(x :: value :: []) :: t :: []) ->
     let e', k = parse e x in
     let e'', v = parse e' value in
     (k,v) :: parse_args e'' t
