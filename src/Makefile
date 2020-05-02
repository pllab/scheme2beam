.PHONY: default
default: _main

_main: lexer.cmo parser.cmo cerl.cmo main.ml
	ocamlc $^ -o $@

lexer_test: lexer.cmo parser.cmo lexer_test.ml
	ocamlc $^ -o $@

parser_test: lexer.cmo parser.cmo parser_test.ml
	ocamlc $^ -o $@

%.cmo: %.mli %.ml
	ocamlc -c $^

cerl.cmo: cerl.ml
	ocamlc -c $^

lexer.cmo: lexer.ml
	ocamlc -c $^

lexer.ml: parser.cmo lexer.mll
	ocamllex lexer.mll

parser.cmo: cerl.cmo parser.ml
	ocamlc -c cerl.cmo parser.mli parser.ml

parser.ml: parser.mly
	ocamlyacc $<

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
	rm -f *\.cmo \.cmi \
		lexer.ml lexer.mli \
		parser.ml parser.mli \
		lexer_test parser_test \
		_scheme
