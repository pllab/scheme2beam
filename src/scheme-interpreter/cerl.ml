(* Abstract Core Erlang specification
 *
 *)

type cexp = 
    | Alias
    | Apply
    | Binary 
    | Bitstr
    | Call
    | Case of cexp * cexp list  (* cexp list must be Clause *)
    | Catch
    | Clause of cexp list * cexp
    | Cons
    | Fun of string * int * cexp list * cexp  (* name, arity, args, body *)
    | Let of cexp list * cexp * cexp 
    | Letrec
    | Literal
    | Map
    | MapPair
    | Module
    | Primop
    | Receive
    | Seq
    | Try
    | Tuple
    | Values
    | Var of string
    (* base *)
    | Atom of string
    | Int of int

let rec string_of_cexp c =
  match c with
  | Fun(s,i,cl,c) -> s ^ "/" ^ (string_of_int i) ^ " = fun (" ^
                       (List.fold_right (fun x y ->  (string_of_cexp x) ^ y) cl "")
                       ^ ") -> " ^ string_of_cexp c
  (* | Let of cexp list * cexp * cexp  *)
  | Var(s) -> "(Var " ^ s ^ ")"
  | Atom(s) -> "(Atom " ^ s ^ ")"
  | Int(i) -> string_of_int i
