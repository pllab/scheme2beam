(* Primitives. *)

(* Helper function.
 * string -> list of chars *)
let char_list_of_string s =
    let rec loop index l =
        if index < 0 then l
        else loop (index - 1) (s.[index]::l) in
    loop ((String.length s) - 1) []

(* Helper function.
 * list of chars -> string *)
let string_of_char_list l =
    let rec loop current remaining =
        match remaining with
        | [] -> current
        | (c::cs) -> loop (current ^ String.make 1 c) cs in
    loop "" l

(* (+ 1 2)
 * (+ 1 2 3) *)
let addition env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let rec iter current_val remaining =
        match remaining with
        | [] -> current_val
        | (Env.Int i) :: remaining' -> iter (current_val + i) remaining'
        | _ -> raise (Invalid_argument "'+' must only be given integer arguments.")
    in ref (Env.Int (iter 0 arg_vals))

(* ( * 1 2)
 * ( * 1 2 3) *)
let multiplication env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let rec iter current_val remaining =
        match remaining with
        | [] -> current_val
        | (Env.Int i) :: remaining' -> iter (current_val * i) remaining'
        | _ -> raise (Invalid_argument "'*' must only be given integer arguments.")
    in ref (Env.Int (iter 1 arg_vals))

(* (- 2 1)
 * (- 1) *)
let subtraction env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Int i] -> Env.Int (- i)  (* Unary minus. *)
    | [Env.Int a; Env.Int b] -> Env.Int (a - b)
    | _ -> raise (Invalid_argument "'-' must be given either one (unary minus)
                  or two arguments.") in
    ref v

(* (/ 9 3) *)
let division env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Int a; Env.Int b] -> Env.Int (a / b)
    | _ -> raise (Invalid_argument "'/' must be given exactly two integer
                  arguments.") in
    ref v

(* (display 123) *)
let display env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.String s] ->
            (* Scanf.unescaped is used so that escape sequences (e.g. "\n") are
             * converted to strings of one character instead of two characters. *)
            let unescaped = Scanf.unescaped s in
            print_string unescaped;
            Env.Nil
            (* Display has an unspecified return value, but this implementation
             * returns an empty list (null). *)
    | [value] ->
        let s = Env.string_of_value value in
        print_string s;
        Env.Nil
    | _ -> raise (Invalid_argument "DISPLAY must be given exactly one
                  argument.") in
    ref v

(* Comparison operators for integers. *)
let make_comparison_primitive op =
    fun env macros args ->
        let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
        match arg_vals with
        | [Env.Int a; Env.Int b] -> ref (Env.Bool (op a b))
        | _ -> raise (Invalid_argument "Comparison operator requires exactly two arguments.")

let int_eq = make_comparison_primitive (=)
let int_lt = make_comparison_primitive (<)
let int_gt = make_comparison_primitive (>)
let int_le = make_comparison_primitive (<=)
let int_ge = make_comparison_primitive (>=)

(* Boolean operators. *)
let is_boolean env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Bool _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "BOOLEAN? requires exactly one argument.") in
    ref v

let is_number env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Int _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "NUMBER? requires exactly one argument.") in
    ref v

let is_char env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Char _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "CHAR? requires exactly one argument.") in
    ref v

let is_string env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.String _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "STRING? requires exactly one argument.") in
    ref v

let is_list env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [x] ->
        let rec loop rem =
            (match rem with
            | Env.Nil -> Env.Bool true
            | Env.Cons (car, cdr) -> loop !cdr
            | _ -> Env.Bool false) in
        loop x
    | _ -> raise (Invalid_argument "LIST? requires exactly one argument.") in
    ref v

let is_pair env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Cons _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "PAIR? requires exactly one argument.") in
    ref v

let is_symbol env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Id _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "SYMBOL? requires exactly one argument.") in
    ref v

let is_null env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Nil] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "NULL? requires exactly one argument.") in
    ref v

let is_vector env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Vector _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "VECTOR? requires exactly one argument.") in
    ref v

let is_procedure env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Primitive _] | [Env.Lambda _] -> Env.Bool true
    | [_] -> Env.Bool false
    | _ -> raise (Invalid_argument "PROCEDURE? requires exactly one argument.") in
    ref v

let car env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    match arg_vals with
    | [Env.Cons (car, _)] -> car
    | _ -> raise (Invalid_argument "CAR must be given a pair.")

let cdr env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    match arg_vals with
    | [Env.Cons (_, cdr)] -> cdr
    | _ -> raise (Invalid_argument "CDR must be given a pair.")

let cons env macros args =
    let arg_vals = List.map (fun a -> Eval.eval a env macros) args in
    match arg_vals with
    | [car; cdr] ->
        ref (Env.Cons (car, cdr))
    | _ -> raise (Invalid_argument "CONS must be given exactly two arguments.")

let is_eqv_helper r1 r2 =
    let v1 = !r1 in
    let v2 = !r2 in
    (match v1, v2 with
    | Env.Bool a, Env.Bool b -> a = b
    | Env.Id a, Env.Id b -> a = b
    | Env.Int a, Env.Int b -> a = b
    | Env.Char a, Env.Char b -> a = b
    | Env.Nil, Env.Nil -> true
    | _ -> r1 == r2)  (* Compare references (pointers). *)

let is_eqv env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in v) args in
    let v = match arg_vals with
    | [r1; r2] -> Env.Bool (is_eqv_helper r1 r2)
    | _ -> failwith "Equality comparators require exactly two arguments." in
    ref v

let is_equal env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in v) args in
    let rec is_equal' r1 r2 =
        let v1 = !r1 in
        let v2 = !r2 in
        (match v1, v2 with
        | Env.String a, Env.String b -> a = b
        | Env.Cons (car1, cdr1), Env.Cons (car2, cdr2) ->
            (!car1 = !car2) && is_equal' cdr1 cdr2
        | Env.Vector v1, Env.Vector v2 ->
            List.for_all2
                (fun a b ->
                    is_equal' a b)
                (Array.to_list v1)
                (Array.to_list v2)
        | _ -> is_eqv_helper r1 r2) in
    match arg_vals with
    | [r1; r2] -> ref (Env.Bool (is_equal' r1 r2))
    | _ -> failwith "Equality comparators require exactly two arguments."

let make_vector env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Int n] -> Env.Vector (Array.make n (ref (Env.Id "banana!")))
    | [Env.Int n; fill] -> Env.Vector (Array.make n (ref fill))
    | _ -> raise (Invalid_argument "MAKE-VECTOR requires exactly one size
                  argument.") in
    ref v

let vector_length env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Vector v] -> Env.Int (Array.length v)
    | _ -> raise (Invalid_argument "VECTOR-LENGTH requires exactly one vector
                  argument.") in
    ref v

let vector_ref env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    match arg_vals with
    | [Env.Vector v; Env.Int i] -> v.(i)
    | _ -> raise (Invalid_argument "VECTOR-REF requires one vector argument,
                  and one index argument.")

let vector_set env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Vector v; Env.Int i; obj] ->
            v.(i) <- ref obj;
            Env.Id "banana!"  (* Return value is unspecified. *)
    | _ -> raise (Invalid_argument "VECTOR-SET! requires one vector argument,
                  one index argument, and one object argument.") in
    ref v

let error env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    match arg_vals with
    | [Env.String s] -> failwith ("[ERROR] " ^ s)
    | _ -> raise (Invalid_argument "ERROR requires exactly one string argument.")
    (* Consider supporting (error "message" 'thing) too. *)

(* number->string *)
let number_to_string env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Int i] -> Env.String (string_of_int i)
    | _ -> raise (Invalid_argument "NUMBER->STRING requires exactly one number argument.") in
    ref v

(* string->number *)
let string_to_number env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.String s] -> Env.Int (int_of_string s)
    | _ -> raise (Invalid_argument "STRING->NUMBER requires exactly one string argument.") in
    ref v

(* char->string *)
let char_to_string env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Char c] -> Env.String (String.make 1 c)
    | _ -> raise (Invalid_argument "CHAR->STRING requires exactly one char argument.") in
    ref v

(* string-> char *)
let string_to_char env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.String s] ->
        (match char_list_of_string s with
        | [c] -> Env.Char c
        | _ -> raise (Invalid_argument "STRING->CHAR string must be of length one."))
    | _ -> raise (Invalid_argument "STRING->CHAR requires exactly one string argument.") in
    ref v

(* string->list
 * String to a list of characters. *)
let string_to_list env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.String s] ->
        let char_list = char_list_of_string s in
        let rec loop rem =
            (match rem with
             | [] -> Env.Nil
             | x::xs -> Env.Cons (ref (Env.Char x), ref (loop xs))) in
        loop char_list
    | _ -> raise (Invalid_argument "STRING->LIST requires exactly one string argument.") in
    ref v

(* list->string
 * List of characters to a string. *)
let list_to_string env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [x] ->
        let rec loop acc rem =
            (match rem with
             | Env.Nil ->
                acc
             | Env.Cons ({contents=Env.Char c}, cdr) ->
                loop (c::acc) !cdr
             | _ ->
                raise (Invalid_argument "LIST->STRING requires a list of characters.")) in
        let char_list = List.rev (loop [] x) in
        Env.String (string_of_char_list char_list)
    | _ -> raise (Invalid_argument "LIST->STRING requires exactly one list argument.") in
    ref v

(* symbol->string *)
let symbol_to_string env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.Id s] -> Env.String s
    | _ -> raise (Invalid_argument "SYMBOL->STRING requires exactly one symbol argument.") in
    ref v

(* string->symbol *)
let string_to_symbol env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    let v = match arg_vals with
    | [Env.String s] -> Env.Id s
    | _ -> raise (Invalid_argument "STRING->SYMBOL requires exactly one string argument.") in
    ref v

let set_var env macros args =
    (match args with
    | [Env.Id id; v] ->
        let evaluated_v = !(Eval.eval v env macros) in
        Env.set env id evaluated_v
    | _ -> raise (Invalid_argument "SET! requires exactly one id argument, and
                  one value argument."));
    ref (Env.Id "banana")  (* Undefined return value. *)

let exit_scm env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    match arg_vals with
    | [] | [Env.Bool true] -> exit 0
    | [Env.Bool false] -> exit 1
    | [Env.Int n] -> exit n
    | _ -> raise (Invalid_argument "EXIT only takes #t, #f, or an integer as
                  argument.")

(* Load a Scheme file.
 * Might modify the environment. *)
let load_scm_file env macros filepath =
    let file = open_in filepath in
    let lexbuf = Lexing.from_channel file in
    let rec loop () =
        let sexpr = (Parser.parse Lexer.lex) lexbuf in 
        match sexpr with
        | None -> ()
        | Some s -> 
           let expr = Env.value_of_sexpr s in
           print_string (Env.string_of_value expr);
            print_string (Cerl.string_of_cexp (Eval.ir expr env));
            loop () in
    (* Change the current working directory.
     * Important for (load ...) when a relative path is given. *)
    let original_cwd = Sys.getcwd () in
    Sys.chdir (Filename.dirname filepath);
    (* Evaluate file. *)
    loop ();
    close_in file;
    (* Restore the current working directory. *)
    Sys.chdir original_cwd
(* WARN: catch errors and close file properly. *)

let load env macros args =
    let arg_vals = List.map (fun a -> let v = Eval.eval a env macros in !v) args in
    match arg_vals with
    | [Env.String path] ->
        load_scm_file env macros path;
        ref (Env.Id "banana!") (* Unspecified return value. *)
    | _ -> raise (Invalid_argument "LOAD only takes one string argument.")


(* For loading primitives into an environment.
 * These are primitives implemented in OCaml.
 * There may be other primitives implemented in Scheme. *)
let load_primitives env =
    let primitives = [
        ("+", addition);
        ("-", subtraction);
        ("*", multiplication);
        ("/", division);
        ("=", int_eq);
        ("<", int_lt);
        (">", int_gt);
        ("<=", int_le);
        (">=", int_ge);
        ("boolean?", is_boolean);
        ("number?", is_number);
        ("char?", is_char);
        ("string?", is_string);
        ("list?", is_list);
        ("pair?", is_pair);
        ("symbol?", is_symbol);
        ("null?", is_null);
        ("vector?", is_vector);
        ("procedure?", is_procedure);
        ("car", car);
        ("cdr", cdr);
        ("cons", cons);
        ("eq?", is_eqv);
        ("eqv?", is_eqv);
        ("equal?", is_equal);
        ("make-vector", make_vector);
        ("vector-length", vector_length);
        ("vector-ref", vector_ref);
        ("vector-set!", vector_set);
        ("error", error);
        ("display", display);
        ("number->string", number_to_string);
        ("string->number", string_to_number);
        ("char->string", char_to_string);
        ("string->char", string_to_char);
        ("string->list", string_to_list);
        ("list->string", list_to_string);
        ("symbol->string", symbol_to_string);
        ("string->symbol", string_to_symbol);
        ("set!", set_var);
        ("exit", exit_scm);
        ("load", load);
    ] in
    List.iter
        (fun (name, proc) ->
            Env.add env name (ref (Env.Primitive proc)))
        primitives
