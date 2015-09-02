
all: compile tests

.PHONY: compile
compile:
	@ ocamlfind ocamlopt -package ounit -linkpkg -o unit_test \
		-I src src/problems.ml test/problemsTest.ml

.PHONY: tests
tests:
	@ ./unit_test

.PHONY: clean
clean:
	@ find . -name "*.cm*" -o -name "*.o" -delete
	@ rm -f unit_test
	@ rm -f *.cache
