
# ocamlfind ocamlc -linkpkg -package sexplib cerl.ml cenv.ml sparse.ml

# how to combine $ eval `opam config env` ?

.PHONY: default
default: _s2b

_s2b: cerl.cmo gencerl.cmo cenv.cmo ir.ml sparse.cmo main.ml #primitives.cmo
	ocamlfind ocamlc -linkpkg -package sexplib $^ -o $@

# sparse_test: lexer.cmo parser.cmo parser_test.ml
# 	ocamlc $^ -o $@

%.cmo: %.mli %.ml
	ocamlc -c $^

cerl.cmo: cerl.ml
	ocamlc -c $^

gencerl.cmo: gencerl.ml
	ocamlc -c $^

gencerl_test: cerl.ml gencerl.ml gencerl_test.ml
	ocamlc -o gencerl_test $^

cenv.cmo: cenv.ml cerl.ml gencerl.ml
	ocamlc -c $^

sparse.cmo: cerl.ml cenv.ml sparse.ml
	ocamlfind ocamlc -linkpkg -package sexplib cerl.ml cenv.ml sparse.ml -o sparse

ir_test: cerl.ml gencerl.ml cenv.ml ir.ml ir_test.ml
	ocamlc -o ir_test $^

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
		_s2b \
		sparse \
		gencerl_test
