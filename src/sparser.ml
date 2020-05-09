
open Cerl
open Sexplib

exception ParserError

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let parse_atom(s : string) : Cerl.cexp =
  match s with
  | "#f" -> Atom("false")
  | "#t" -> Atom("true")
  | s when is_int s -> let i = (int_of_string s) in Int(i)
  | s -> Var(s)

(* worrisome mutual recursion! hence the "and" *)
let rec parse_args(s: Sexp.t) : (Cerl.cexp * Cerl.cexp) list =
  match s with
  | Sexp.List([]) -> []
  (* the atom here has to be a var though... *)
  | Sexp.List(Sexp.List(x :: value :: []) :: []) ->
     let k = parse x in
     let v = parse value in
     [(k,v)]
  | Sexp.List(Sexp.List(x :: value :: []) :: t :: []) ->
     let k = parse x in
     let v = parse value in
     (k,v) :: parse_args t
  and
parse(s : Sexp.t) : Cerl.cexp =
  match s with
  | Sexp.Atom(a) -> parse_atom a
  | Sexp.List(Sexp.Atom("if") :: guard :: true_branch :: false_branch :: []) ->
     let g = parse guard in 
     let t = parse true_branch in
     let f = parse false_branch in 
     Case([],
          [ Clause([], g, t) ;
            Clause([], Atom("true"), f)
       ])
  | Sexp.List(Sexp.Atom("let") :: arg_map :: body :: []) ->
     let arg_list = parse_args arg_map in
     let b = parse body in
     (* right now just pull out the first mapping *)
     let (a,e) = match arg_list with
       | [] -> (Atom("empty"),Atom("empty"))  (* also need to do this better *)
       | (aa,ee)::t -> (aa,ee)
     in
     Let([a],e,b)
  | _ -> raise ParserError

let rec tokenize(s : string) : Sexp.t = Sexp.of_string s 

(* let () =
 *   let tokens = tokenize "(let ((x 4)) x)" in print_string (Cerl.string_of_cexp (parse tokens)  ^ "\n" ) *)


