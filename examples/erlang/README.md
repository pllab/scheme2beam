* Visualizations
** et_viewer
At the shell, running the following will bring up a gui that visualizes message passing:

```
$ erl
$ c(tut.erl).
$ tut:test().
```

It would be nice to use this facility for visualizing the Milner-translated programs.

http://erlang.org/doc/apps/et/et_tutorial.html

https://www.tutorialspoint.com/erlang/erlang_processes.htm

** send_rev example
Managed to instrument the basic send_recv example with the et_viewer functions.

```
$ erl
$ c(tut.erl).
$ send_recv:run().
```
