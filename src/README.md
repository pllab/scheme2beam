
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
make gencerl_test
./gencerl_test
```

## Miscellaneous Notes about doing this in the Ocaml toplevel
Alternatively, in an interactive shell (ensuring you've eval'ed the opam environment as above):
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

# #trace parse;;

# let x = Sexp.of_string "(this)";;
- : Sexplib.Sexp.t = Sexplib.Sexp.List [Sexplib.Sexp.Atom "this"]

# match x with 
   | Sexplib.Sexp.Atom s -> "full"
   | Sexplib.Sexp.List [ Sexplib.Sexp.Atom s ] -> "empty";;

# let root_env = Cenv.make_env None;;

# Sparse.parse root_env x;;

```

```
$ ocamlfind ocamlc -linkpkg -package sexplib pcf.ml parser.ml -o parser_example.byte
```

## Example: running a generated Core Erlang module

Let's say you generated some Core Erlang from the `gen_cerl` module and you want to run it. 
As an example, you could run the `gen_cerl` test suite and grab the output of one of the unit tests for a module called `my_factorial` and paste that into a file called `my_factorial.core`.

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


## Milner's Translation

### Pi Calculus in Erlang

The translation by Vasconcelos and others more or less agree on how to translate
the lambda calculus into pi calculus.  Having worked through translation on the
identity function in Erlang to get a running version, the basic skeleton of
evaluation appears fairly straightforward.  The code in `id.erl` essentially
comprises this skeleton.

The compiled `id.erl` to `id.core` is our target for translating our `cerl` IR
into a concurrent/message passing version of the same code.

### Algorithm Skeleton and Example

```
((define id(x) x) "hey")

  |
  v
// internal
id["id" -> Fun("id", 1, [Var("x")], Var("x"))] 

Apply("id", 1, [Atom("hey")])

  |
  v

Core Erlang
```

From the above picture, we would like to:

1. spawn func and arg procs (A, B)
   - A gets Apply's left value as its function
   - B gets A.func_recv
2. send Apply's right arg to A with B
3. create the A and B functions
   - B_recv we get for free, it just receives 
     a single param representing final value
   - A_func identical form in example

4. which only leaves the eval function over an 
   arbitrary cerl term

We note that the eval function is the "missing piece" to compiling more
complicated scheme into Core Erlang.


### TODO

1. [ ] "IR pass" to take an Apply to the above racket
   - focus on identity (since that's all we've done)
   - above "algorithm", dismantle Apply to fill in above
   - done by Sunday, May 31
   - tracing for great good (visualizations)

2. [ ] general eval function to get beyond id
   - arithmetic
   - done by June 8

3. [ ] multiple/nested functions
   - pretty traces
   - ?

4. [ ] write paper 

