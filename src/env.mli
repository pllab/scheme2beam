type id = string

(* Value of data in environments. *)
type value =
    | Bool of bool
    | Int of int
    | Real of float
    | Char of char
    | String of string
    | Id of string
    | Cons of value ref * value ref
    | Nil
    | Vector of value ref array
    | Primitive of (env -> macro_table -> value list -> value ref)  (* Procedures defined using OCaml. *)
    (* (lambda <id list> <Ast.expr list>) *)
    | Lambda of env * value * value list  (* User-defined procedures. *)
and macro_table = (string, value) Hashtbl.t
and env = {
    parent : env option;  (* Parent frame of this environment. *)
    bindings : (id, value ref) Hashtbl.t;
}


val value_of_sexpr : Sexpr.sexpr -> value

val string_of_value : value -> string
val string_of_value_type : value -> string

val make_env : env option -> env
val lookup : env -> id -> value ref
val add : env -> id -> value ref -> unit
val set : env -> id -> value -> unit
