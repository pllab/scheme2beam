
open Pcf
open Sexplib

exception ParserError

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let rec parse_bool(s : Sexplib.Sexp.t) : Pcf.term =
  match s with
  | Sexplib.Sexp.Atom "#t" -> TTrue
  | Sexplib.Sexp.Atom "#f" -> TFalse
  | _ -> raise ParserError

let rec parse_int(s : Sexplib.Sexp.t) : Pcf.term = 
  match s with
  | Sexplib.Sexp.Atom n when is_int n -> let i = (int_of_string n) in TNat(i)
  | _ -> raise ParserError

let rec parse_cond(s : Sexplib.Sexp.t) : Pcf.term =
  match s with
  | Sexplib.Sexp.List [Sexplib.Sexp.Atom "if";
                       g;
                       t;
                       f] -> let gc = parse_bool g in
                             let tb = parse_int t in
                             let fb = parse_int f in
                             TIte(gc,tb,fb)
  | _ -> raise ParserError

let rec parse(s : Sexplib.Sexp.t) : Pcf.term =
  match s with
  | Sexplib.Sexp.Atom a -> parse_bool (Sexplib.Sexp.Atom a)
  | Sexplib.Sexp.List h -> parse_cond (Sexplib.Sexp.List h)
  | _ -> raise ParserError

let rec tokenize(s : string) : Sexplib.Sexp.t = Sexp.of_string s 

let () =
  let tokens = tokenize "(if #f 4 5)" in print_string (Pcf.prettify (parse tokens)  ^ "\n" )
