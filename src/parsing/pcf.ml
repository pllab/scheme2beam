
(* this is a collection of data structures representing the PCF language plus call/cc, which constitutes our abstract syntax *)

type term =
  | TTrue     (* constants *)
  | TFalse
  | TVar of string
  | TNat of int
  | TSucc of term
  | TPred of term
  | TIszero of term
  | TIte of term * term * term
  | TFun of term * term * term
  | TApp of term * term
(* | TUnit *)
(* | TCallcc *)
(* | TFix  *);;


let rec prettify(t : term) : string =
  match t with
  | TTrue -> "true"
  | TFalse -> "false"
  | TVar v -> "(Var " ^ v ^ ")"
  | TNat n -> "(Nat " ^ string_of_int n ^ ")"
  | TIte(guard,tb,fb) -> "if " ^ prettify guard ^ " then " ^ prettify tb ^ " else " ^ prettify fb
  (* | TSucc 
   * | TPred of term
   * | TIszero of term
   * | TFun of term * term * term
   * | TApp of term * term *)
    
