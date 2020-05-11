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

    (* Modules: name, exports, attributes (Tuple of Attribute), definitions (list of Fun)
     *   module Name [E1, ..., Ek]
     *     attributes [K1 = T1, ...,
     *                 Km = Tm]
     *     V1 = F1
     *     ...
     *     Vn = Fn
     *   end
     *
     * where Exports = [E1, ..., Ek],
     * Attributes = [{K1, T1}, ..., {Km, Tm}],
     * and Definitions = [{V1, F1}, ..., {Vn, Fn}].
     *
     * Name and all the Ki must be atom literals, and all the Ti must be constant
     * literals. All the Vi and Ei must have type var and represent function 
     * names. All the Fi must have type 'fun'. 
     *)
    | Module of string * cexp list * cexp list * cexp list 
    (* Module helpers *)
    | Export of string * int
    | Attribute of cexp * cexp

    | Primop of string * cexp list
    | Receive
    | Seq
    | Try
    | Tuple of cexp list
    | Values of cexp list
    | Var of string
    (* base *)
    | Atom of string
    | Int of int
