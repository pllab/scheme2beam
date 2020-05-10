
# ocamlfind ocamlc -linkpkg -package sexplib cerl.ml env.ml sparse.ml

.PHONY: default
default: _s2b

_s2b: cerl.cmo env.cmo sparse.cmo gencerl.ml main.ml #primitives.cmo
	ocamlfind ocamlc -linkpkg -package sexplib $^ -o $@

# sparse_test: lexer.cmo parser.cmo parser_test.ml
# 	ocamlc $^ -o $@

%.cmo: %.mli %.ml
	ocamlc -c $^

cerl.cmo: cerl.ml
	ocamlc -c $^

gencerl.cmo: gencerl.ml
	ocamlc -c $^

env.cmo: env.ml
	ocamlc -c $^

sparse.cmo: cerl.ml env.ml sparse.ml
	ocamlfind ocamlc -linkpkg -package sexplib cerl.ml env.ml sparse.ml

.PHONY: test
# test: _main
# 	@echo "Scheme expressions."
# 	./scheme src-scheme/test-expressions.scm
# 	@echo "Primitives implemented in OCaml."
# 	./scheme src-scheme/test-primitives-ocaml.scm
# 	@echo "Primitives implemented in Scheme."
# 	./scheme src-scheme/test-primitives.scm

.PHONY: clean
clean:
	rm -f *\.cmo *\.cmi \
		_s2b
