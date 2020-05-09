(* Abstract Core Erlang specification
 *
 *)

type cexp = 
    | Alias
    | Apply
    | Binary 
    | Bitstr
    | Call
    | Case of cexp list * cexp list  (* second cexp list must be Clause *)
    | Catch
    | Clause of cexp list * cexp * cexp
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
  | Var(s) -> s
  | Atom(s) -> "'" ^ s ^ "'"
  | Int(i) -> string_of_int i
  | Clause(cexp_list, cexp1, cexp2) -> " <" ^
                                         (List.fold_right (fun x y -> (string_of_cexp x) ^ "," ^ y) cexp_list "") ^
                                           ">" ^ " when " ^
                                             string_of_cexp cexp1 ^ " -> " ^
                                               string_of_cexp cexp2
  | Case(cl1, cl2) -> "case <" ^ (List.fold_right (fun x y -> (string_of_cexp x) ^ "," ^ y) cl1 "") ^ ">" ^
                        " of " ^
                          (List.fold_right (fun x y -> (string_of_cexp x) ^ y) cl2 "")
