# Bytecode notes

Erlang source compiler flows like this:

```
Erlang → Erlang Abstract Format → Core Erlang → BEAM Bytecode
```

## Core Erlang

Core Erlang is a simplified "core" or "subset" of Erlang. It is an intermediate representation used for optimizations and is well-suited for program analyses and transformations. This is what [LFE](https://github.com/rvirding/lfe/) (Lisp Flavored Erlang) compiles to. 

Most of Core Erlang consists of functions, case statements, guards, and let bindings; this likely lends itself well as a target for our Scheme source compiler.

To extract Core Erlang from an Erlang module:

```
 $ erlc +to_core my_module.erl
```

To compile a Core Erlang file:

```
 $ erlc my_module.core
```

## Erlang Abstract Format

Erlang compiler front-end passes operate over the abstract format. It's pretty close to the original Erlang source code, and for that reason likely not a suitable target for our Scheme source compiler. 

# Resources

- [The Core of Erlang](https://8thlight.com/blog/kofi-gumbs/2017/05/02/core-erlang.html)
	- Good intro to Core Erlang.
- [Core Erlang by example](http://blog.erlang.org/core-erlang-by-example/)
	- Very helpful for understanding how Erlang gets transformed to Core Erlang.
- [Erlang abstract format](http://erlang.org/doc/apps/erts/absform.html)
- [Implementing languages on the BEAM (video)](https://www.youtube.com/watch?v=qm0mbQbc9Kc)
- [Core Erlang source](https://github.com/erlang/otp/blob/master/lib/compiler/src/cerl.erl)
	- Unfortunately, reading the source code for the `cerl` module is the closest thing we have to an up-to-date "specification" for Core Erlang. But due to the simplicity of the IR, this isn't terrible. It just may make debugging malformed Core Erlang a little tricky. Luckily there seems to be a good amount of other resources about Core Erlang out there. 

