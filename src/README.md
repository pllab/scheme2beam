
# Minimal Parsing
## Lexing with Sexps
This is some code illustrating JaneStreet's sexplib parser, which takes a string to an S-expression.
A minimal example may be created by 
```$ ocamlfind ocamlc -linkpkg -package sexplib sexp.ml  -o sexp_example.byte```
and run according to
``` $ ./sexp_example```

Alternatively, in an interactive shell:

```
# #use topfind;;
# #require "sexplib";;
# open Sexplib;;
# let x = Sexp.of_string "(this)";;
- : Sexplib.Sexp.t = Sexplib.Sexp.List [Sexplib.Sexp.Atom "this"]

match x with 
  | Sexplib.Sexp.Atom s -> "full"
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom s ] -> "empty";;
```

## Parsing
I tried including a slightly more involved example with the `pcf.ml` and `parser.ml` files.  However,
to parse scheme we will most likely need recourse to a parser generator (and lexer), such as ocamllex
and ocamlyacc, or we will need to roll our own and get really clever.  Nevertheless, the example may
be built according to:

```$ ocamlfind ocamlc -linkpkg -package sexplib pcf.ml parser.ml -o parser_example.byte```