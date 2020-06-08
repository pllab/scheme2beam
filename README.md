# Scheme2Beam

## Overview

This project is a source compiler that takes programs written in Scheme and compiles them to run on the Erlang VM, BEAM. 

## Depedencies

- Erlang
- OCaml
- Jane Street's sexplib (can install through `opam`)

## Building
This guide assumes you have set up an `opam` repository.  And if you don't already
have Janestreet's Sexplib installed, you may run the following:

```
$ opam install sexplib

$ eval `opam config env`
```
Then, in `src/`, run `make`. 
```
$ make
```

## Example

To compile a Scheme program, run:
```
$ ./s2b tests/factorial_test.scm > factorial-test.core
```
More tests are listed in the `tests`
directory.

To compile a Scheme program and run our parallelization pass, run:
```
$ ./s2b tests/id2.scm -m > id2.core 
```

### Run on the BEAM

To run on the BEAM, compile the `.core` files down to BEAM bytecode by running, for example:
```
$ erlc factorial_test.core
```
Then, in the same directory, open up the command-line interpreter for Erlang, `erl` and run:
```
> factorial_test:factorial(5).
```

