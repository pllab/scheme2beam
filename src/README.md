
# How to Run
```
# if you don't already have Janestreet's Sexplib installed
$ opam install sexplib

$ eval `opam config env`
$ make
```

## Main
Assuming you're sitting in project root:
```
$ ./s2b `pwd`/tests/if_test.scm
```

## Tests
```
./gencerl_test
```

## Miscellaneous Notes about doing this in the Ocaml toplevel
Alternatively, in an interactive shell (ensuring you've eval'ed the opam environment as above):
```
# #use topfind;;
# #require "sexplib";;
# open Sexplib;;
# #load "cerl.cmo";;
# open Cerl;;
# #load "sparser.cmo";;
# open Sparser;;

# let x = Sexp.of_string "(this)";;
- : Sexplib.Sexp.t = Sexplib.Sexp.List [Sexplib.Sexp.Atom "this"]

match x with 
  | Sexplib.Sexp.Atom s -> "full"
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom s ] -> "empty";;
```

```
$ ocamlfind ocamlc -linkpkg -package sexplib pcf.ml parser.ml -o parser_example.byte
```
