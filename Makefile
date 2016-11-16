OBJS = ast.cmx parser.cmx scanner.cmx codegen.cmx javapm.cmx 

YACC = ocamlyacc

javapm: $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis -o javapm $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	$(YACC) -v parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -c -package llvm $<

.PHONY: clean
clean:
	rm -f javapm parser.ml parser.mli scanner.ml \
	    *.cmo *.cmi *.out *.diff *.output javapm *.dSYM

.PHONY: all
all: clean javapm

ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
javapm.cmo : scanner.cmo parser.cmo codegen.cmo ast.cmo
javapm.cmx : scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmi : ast.cmo
