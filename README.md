# Scheme2Beam

## Overview

Overall we plan to compile a subset of the scheme language to the Erlang virtual machine, the BEAM.  We have chosen scheme
because it is has a straightforward semantics, close to the lambda calculus, which will hopefully make porting it easier than
a more fully featured language such as Java.

## Milestones

  1. Write a few programs in Erlang and dump the bytecode
     We will focus on very simple programs at first to get a feel for the bytecode.
  2. Choose a language for the source compiler.
     Examples exist using Rust, we've learned:
     https://github.com/gleam-lang/gleam
     But we may also choose either scheme or erlang itself.
  3. Begin targeting a translation from scheme.
     This at the moment is largely research-oriented, as neither of us yet feel
     confident we know how to proceed much pas this point.
  .
  .
  .
  10?  Hopefully we can add concurrency features.
       Interesting approaches are here:
       https://www.sciencedirect.com/science/article/pii/S030439750600555X
       and
       here:
       https://arxiv.org/pdf/1312.2702.pdf
       