
open Cerl
open Env
open Sexplib

exception ParserError

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let parse_atom(e: env) (s : string) : (env * Cerl.cexp) =
  match s with
  | "#f" -> (e, Atom("false"))
  | "#t" -> (e, Atom("true"))
  | s when is_int s -> let i = (int_of_string s) in (e, Int(i))
  | s -> let v = Env.lookup s e in (e, v)

(* let parse_func(name: Sexp.t) (args: Sexp.t) (body: Sexp.t) : cexp =
 *   let rec aux arglist =
 *     match arglist with
 *     | Sexp.List([]) -> []
 *     | Sexp.List(h::t) -> (parse h) :: aux t
 *   in
 *   let b = parse b
 *   in
 *   let a = aux args
 *   in Fun(name, List.length a, a, b) *)

(* worrisome mutual recursion! hence the "and" *)
(* let rec parse_args(s: Sexp.t) : (Cerl.cexp * Cerl.cexp) list =
 *   match s with
 *   | Sexp.List([]) -> []
 *   (\* the atom here has to be a var though... *\)
 *   | Sexp.List(Sexp.List(x :: value :: []) :: []) ->
 *      let k = parse x in
 *      let v = parse value in
 *      [(k,v)]
 *   | Sexp.List(Sexp.List(x :: value :: []) :: t :: []) ->
 *      let k = parse x in
 *      let v = parse value in
 *      (k,v) :: parse_args t
 *   and *)

let rec parse(e: Env.env) (s: Sexp.t): (Env.env * Cerl.cexp) =
  match s with
  | Sexp.Atom(a) -> parse_atom e a
  | Sexp.List(Sexp.Atom("if") :: guard :: true_branch :: false_branch :: []) ->
     let e', g = parse e guard in 
     let e', t = parse e true_branch in
     let e', f = parse e false_branch in 
     (e, Case(Values([]),
              [ Clause(Values([]), g, t) ;
                Clause(Values([]), Atom("true"), f)
           ]))
      
  (*  | Sexp.List(Sexp.Atom("let") :: arg_map :: body :: []) ->
  *     let arg_list = parse_args arg_map in
  *     let b = parse body in
  *     (* right now just pull out the first mapping *)
  *     let (a,e) = match arg_list with
  *       | [] -> (Atom("empty"),Atom("empty"))  (* also need to do this better *)
  *       | (aa,ee)::t -> (aa,ee)
  *     in
  *     Let([a],e,b)
  * 
  *  | Sexp.List(Sexp.Atom("define") :: Sexp.Atom(name) :: arg_list :: body :: []) ->
  *     let cdef = (parse_func name arg_list body) in Env.add name cdef e in (e, cdef)                                                   
  * 
  * (* not sure where scheme's lambda goes; this but a random name? *)
  * |  Sexp.List(Sexp.Atom("lambda") :: arg_list :: body :: []) ->
  *     parse_func "" arg_list body *)

  | _ -> raise ParserError


