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

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sexpr.sexpr option
