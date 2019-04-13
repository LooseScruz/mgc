# "make mgc.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

mgc.native : mgc.ml Lex_TabCount.ml parser.mly scanner.mll sast.ml semant.ml codegen.ml ast.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind mgc.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	opam config exec -- \
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff

