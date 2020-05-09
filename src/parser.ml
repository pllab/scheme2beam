
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

let rec parse(s : Sexp.t) : Cerl.cexp =
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
  | _ -> raise ParserError

let rec tokenize(s : string) : Sexp.t = Sexp.of_string s 

let () =
  let tokens = tokenize "(if #f 4 5)" in print_string (Cerl.string_of_cexp (parse tokens)  ^ "\n" )


