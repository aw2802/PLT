OBJS = ast.cmx sast.cmx parser.cmx scanner.cmx utils.cmx semant.cmo codegen2.cmx javapm.cmx

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
	    *.cmo *.cmi *.cmx *.o *.out *.diff *.output javapm *.dSYM

.PHONY: all
all: clean javapm

ast.cmo :
ast.cmx :
sast.cmo : 
sast.cmx :
utils.cmo : ast.cmo
utils.cmx : ast.cmx 
codegen2.cmo : ast.cmo sast.cmo utils.cmo
codegen2.cmx : ast.cmx sast.cmx utils.cmx
parser.cmo : ast.cmo sast.cmo parser.cmi
parser.cmx : ast.cmx sast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : sast.cmi ast.cmo
semant.cmx : sast.cmi ast.cmx
javapm.cmo : scanner.cmo parser.cmo codegen2.cmo ast.cmo sast.cmo utils.cmo
javapm.cmx : scanner.cmx parser.cmx codegen2.cmx ast.cmx sast.cmx utils.cmo
parser.cmi : ast.cmo 
