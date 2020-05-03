(* Abstract Core Erlang specification
 *
 *)

type cexp =
    | Alias
    | Apply of string * int * cexp list
    | Binary
    | Bitstr
    | Call of string * string * cexp list
    | Case of cexp * cexp list (* cexp list must be Clause *)
    | Catch
    | Clause of cexp * cexp * cexp
    | Cons
    | Fun of string * int * cexp list * cexp (* name, arity, args, body *)
    | Let of cexp * cexp * cexp
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
    | Values of cexp list
    | Var of string
    (* base *)
    | Atom of string
    | Int of int
