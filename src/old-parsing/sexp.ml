
open Sexplib

exception Blarg

open Sexplib

let () =
  (* Build an Sexp from: (This (is an) (s expression)) *)
  let exp1 = Sexp.(List [
    Atom "This";
    List [Atom "is"; Atom "an"];
    List [Atom "s"; Atom "expression"]
  ]) in 
  (* Serialize an Sexp object into a string *)
  print_endline (Sexp.to_string exp1);
  (* Parse a string and produce a Sexp object  *)
  let exp2 = Sexp.of_string "(This (is an) (s expression))" in
  (* Ensure we parsed what we expected. *)
  assert (Sexp.compare exp1 exp2 = 0)
  
