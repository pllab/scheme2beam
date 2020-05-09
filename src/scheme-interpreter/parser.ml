type token =
  | LPAREN
  | RPAREN
  | LSPAREN
  | RSPAREN
  | DOT
  | BOOL of (bool)
  | INT of (int)
  | REAL of (float)
  | ID of (string)
  | CHAR of (char)
  | STRING of (string)
  | QUOTE
  | QUASIQUOTE
  | UNQUOTE
  | HASH
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
  (* Header. *)
let rec cons_of_list l last =
    match l with
    | [] -> last  (* The last element at the end of the chain of conses. *)
    | x::xs -> Sexpr.Cons (x, cons_of_list xs last)
# 28 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LSPAREN *);
  260 (* RSPAREN *);
  261 (* DOT *);
  268 (* QUOTE *);
  269 (* QUASIQUOTE *);
  270 (* UNQUOTE *);
  271 (* HASH *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  262 (* BOOL *);
  263 (* INT *);
  264 (* REAL *);
  265 (* ID *);
  266 (* CHAR *);
  267 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\006\000\
\007\000\008\000\004\000\004\000\004\000\004\000\009\000\009\000\
\010\000\010\000\010\000\010\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\005\000\005\000\
\003\000\003\000\004\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\010\000\011\000\012\000\015\000\
\013\000\014\000\000\000\000\000\000\000\000\000\002\000\031\000\
\001\000\003\000\007\000\004\000\005\000\006\000\008\000\009\000\
\019\000\029\000\000\000\020\000\000\000\016\000\017\000\018\000\
\000\000\000\000\021\000\000\000\030\000\022\000\000\000\025\000\
\000\000\026\000\000\000\000\000\000\000\027\000\028\000\023\000\
\024\000"

let yydgoto = "\002\000\
\016\000\026\000\018\000\019\000\027\000\020\000\021\000\022\000\
\023\000\024\000"

let yysindex = "\003\000\
\001\000\000\000\072\255\087\255\000\000\000\000\000\000\000\000\
\000\000\000\000\162\255\162\255\162\255\002\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\042\255\000\000\057\255\000\000\000\000\000\000\
\102\255\117\255\000\000\162\255\000\000\000\000\162\255\000\000\
\132\255\000\000\147\255\004\255\003\255\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\000\000\254\255\000\000\000\000\000\000\
\000\000\000\000"

let yytablesize = 272
let yytable = "\017\000\
\015\000\029\000\033\000\001\000\034\000\048\000\049\000\000\000\
\000\000\030\000\031\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\000\000\037\000\000\000\000\000\041\000\043\000\
\000\000\000\000\044\000\000\000\000\000\045\000\000\000\037\000\
\000\000\037\000\003\000\035\000\004\000\000\000\036\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\014\000\003\000\000\000\004\000\038\000\039\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\003\000\025\000\004\000\000\000\000\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\014\000\003\000\
\000\000\004\000\028\000\000\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\003\000\040\000\
\004\000\000\000\000\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\003\000\000\000\004\000\
\042\000\000\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\003\000\046\000\004\000\000\000\
\000\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\014\000\003\000\000\000\004\000\047\000\000\000\
\005\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\003\000\000\000\004\000\000\000\000\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\012\000\013\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\004\000\000\000\000\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000"

let yycheck = "\001\000\
\000\000\004\000\001\001\001\000\003\001\002\001\004\001\255\255\
\255\255\011\000\012\000\013\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\027\000\255\255\029\000\255\255\255\255\033\000\034\000\
\255\255\255\255\036\000\255\255\255\255\039\000\255\255\041\000\
\255\255\043\000\001\001\002\001\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\001\001\255\255\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\001\001\002\001\003\001\255\255\255\255\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\001\001\
\255\255\003\001\004\001\255\255\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\001\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\001\001\255\255\003\001\
\004\001\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\001\001\002\001\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\001\001\255\255\003\001\004\001\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LSPAREN\000\
  RSPAREN\000\
  DOT\000\
  QUOTE\000\
  QUASIQUOTE\000\
  UNQUOTE\000\
  HASH\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL\000\
  INT\000\
  REAL\000\
  ID\000\
  CHAR\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 34 "parser.mly"
             ( Some _1 )
# 200 "parser.ml"
               : Sexpr.sexpr option))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
        ( None )
# 206 "parser.ml"
               : Sexpr.sexpr option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 38 "parser.mly"
            ( _1 )
# 213 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'quoted) in
    Obj.repr(
# 39 "parser.mly"
           ( Sexpr.Cons (Sexpr.Id "quote", Sexpr.Cons (_1, Sexpr.Nil)) )
# 220 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'quasiquoted) in
    Obj.repr(
# 40 "parser.mly"
                ( Sexpr.Cons (Sexpr.Id "quasiquote", Sexpr.Cons (_1, Sexpr.Nil)) )
# 227 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unquoted) in
    Obj.repr(
# 41 "parser.mly"
             ( Sexpr.Cons (Sexpr.Id "unquote", Sexpr.Cons (_1, Sexpr.Nil)) )
# 234 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr list) in
    Obj.repr(
# 42 "parser.mly"
          ( cons_of_list _1 Sexpr.Nil )
# 241 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dotted_slist) in
    Obj.repr(
# 43 "parser.mly"
                 ( match _1 with (l, e) -> cons_of_list l e )
# 248 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vector) in
    Obj.repr(
# 44 "parser.mly"
           ( Sexpr.Vector _1 )
# 255 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 47 "parser.mly"
           ( Sexpr.Bool _1 )
# 262 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "parser.mly"
        ( Sexpr.Int _1 )
# 269 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 49 "parser.mly"
         ( Sexpr.Real _1 )
# 276 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 50 "parser.mly"
         ( Sexpr.Char _1 )
# 283 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
           ( Sexpr.String _1 )
# 290 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
       ( Sexpr.Id _1 )
# 297 "parser.ml"
               : Sexpr.sexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 55 "parser.mly"
                    ( _2 )
# 304 "parser.ml"
               : 'quoted))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 58 "parser.mly"
                              ( _2 )
# 311 "parser.ml"
               : 'quasiquoted))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 61 "parser.mly"
                        ( _2 )
# 318 "parser.ml"
               : 'unquoted))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                     ( [] )
# 324 "parser.ml"
               : Sexpr.sexpr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                    ( [] )
# 330 "parser.ml"
               : Sexpr.sexpr list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr list) in
    Obj.repr(
# 67 "parser.mly"
                             ( _2 )
# 337 "parser.ml"
               : Sexpr.sexpr list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr list) in
    Obj.repr(
# 68 "parser.mly"
                               ( _2 )
# 344 "parser.ml"
               : Sexpr.sexpr list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Sexpr.sexpr list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr) in
    Obj.repr(
# 71 "parser.mly"
                                                 ( (_2, _4) )
# 352 "parser.ml"
               : 'dotted_slist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Sexpr.sexpr list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr) in
    Obj.repr(
# 72 "parser.mly"
                                         ( (_2, _4) )
# 360 "parser.ml"
               : 'dotted_slist))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                           ( [||] )
# 366 "parser.ml"
               : 'vector))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                         ( [||] )
# 372 "parser.ml"
               : 'vector))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr list) in
    Obj.repr(
# 77 "parser.mly"
                                  ( Array.of_list _3 )
# 379 "parser.ml"
               : 'vector))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr list) in
    Obj.repr(
# 78 "parser.mly"
                                    ( Array.of_list _3 )
# 386 "parser.ml"
               : 'vector))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 84 "parser.mly"
                  ( [_1] )
# 393 "parser.ml"
               : Sexpr.sexpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.sexpr list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.sexpr) in
    Obj.repr(
# 85 "parser.mly"
                     ( _1 @ [_2] )
# 401 "parser.ml"
               : Sexpr.sexpr list))
(* Entry parse *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sexpr.sexpr option)
;;
# 89 "parser.mly"

(* trailer *)
# 429 "parser.ml"
