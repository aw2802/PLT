OBJS = ast.cmx sast.cmx parser.cmx scanner.cmx codegen.cmx javapm.cmx utils.cmx 

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
sast.cmo: 
sast.cmx:
utils.cmo : 
utils.cmx : 
odegen.cmo : ast.cmo sast.cmo utils.cmo
codegen.cmx : ast.cmx sast.cmx utils.cmx
parser.cmo : ast.cmo sast.cmo parser.cmi
parser.cmx : ast.cmx sast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
javapm.cmo : scanner.cmo parser.cmo codegen.cmo ast.cmo sast.cmo utils.cmo
javapm.cmx : scanner.cmx parser.cmx codegen.cmx ast.cmx sast.cmx utils.cmx
parser.cmi : ast.cmo 
