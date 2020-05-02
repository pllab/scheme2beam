(* Representation of s-expressions. *)

type sexpr =
    | Id of string
    | Int of int
    | Real of float
    | Bool of bool
    | Char of char
    | String of string
    | Cons of sexpr * sexpr
    | Nil
    | Vector of sexpr array


let rec string_of_sexpr e =
    match e with
    | Id s -> "ATOM[" ^ s ^ "]"
    | Int i -> "ATOM[" ^ string_of_int i ^ "]"
    | Real r -> "ATOM[" ^ string_of_float r ^ "]"
    | Bool true -> "ATOM[#t]"
    | Bool false -> "ATOM[#f]"
    | Char c -> "ATOM[" ^ String.make 1 c ^ "]"
    | String s -> "ATOM[" ^ s ^ "]"
    | Cons (car, cdr) ->
        "CONS[" ^ string_of_sexpr car ^ " " ^ string_of_sexpr cdr ^ "]"
    | Nil -> "NULL"
    | Vector a ->
        let str_array = List.map (fun e -> string_of_sexpr e) (Array.to_list a) in
        "VECTOR[" ^ String.concat " " str_array ^ "]"
