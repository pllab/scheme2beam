
# How to Run

This guide assumes you have set up an `opam` repository.  And if you don't already
have Janestreet's Sexplib installed, you may run the following:

```
$ opam install sexplib

$ eval `opam config env`
$ make
```

## Main

Assuming you're sitting in project root, the following will compile a simple
conditional expression into Core Erlang.  More tests are listed in the `tests`
directory.

```
$ ./s2b tests/if_test.scm
```

## Tests

There are further tests about actually generating Core Erlang in:

```
make gencerl_test
./gencerl_test
```

## Miscellaneous Notes about doing this in the Ocaml toplevel

You may avoid using the Makefile and follow these directions for working within
an Ocaml toplevel.  Alternatively, in an interactive shell (ensuring you've
eval'ed the opam environment as above):

```
# #use "topfind";;
# #require "sexplib";;
# open Sexplib;;
# #load "cerl.cmo";;
# open Cerl;;
# #load "cenv.cmo";;
# open Cenv;;
# #load "sparse.cmo";;
# open Sparse;;
# ... and all other file dependencies ... 
# #trace parse;;

# let x = Sexp.of_string "(this)";;
- : Sexplib.Sexp.t = Sexplib.Sexp.List [Sexplib.Sexp.Atom "this"]

# match x with 
   | Sexplib.Sexp.Atom s -> "full"
   | Sexplib.Sexp.List [ Sexplib.Sexp.Atom s ] -> "empty";;

# let root_env = Cenv.make_env None;;

# Sparse.parse root_env x;;

```

## Example: running a generated Core Erlang module

Let's say you generated some Core Erlang from the `gen_cerl` module and you want
to run it.  As an example, you could run the `gen_cerl` test suite and grab the
output of one of the unit tests for a module called `my_factorial` and paste
that into a file called `my_factorial.core`.

Then compile it to a BEAM bytecode:

```
$ erlc my_factorial.core
```

This should make a file called `my_factorial.beam`. 
Now let's call our factorial function from another module! 
Here's an example module that calls map on our factorial function:

```erlang
-module (map_test).
-export ([start/0]).

start() ->
    L = lists:seq(1,4),
    M = lists:map(fun(X)->my_factorial:factorial(X) end, L),
    io:format("List is ~p~n", [M]).
```

Compile it with:
```
$ erlc map_test.erl
```

Run with:
```
$ erl -noshell -s map_test start -s init stop
```
And there you go! That called our generated `my_factorial` module!

