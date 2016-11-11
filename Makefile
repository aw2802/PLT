OBJS = ast.cmo parser.cmo scanner.cmo javapm.cmo 

YACC = ocamlyacc

javapm: $(OBJS)
	ocamlc -o javapm $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	$(YACC) -v parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

.PHONY: clean
clean:
	rm -f javapm parser.ml parser.mli scanner.ml \
	    *.cmo *.cmi *.out *.diff *.output javapm *.dSYM

.PHONY: all
all: clean javapm

ast.cmo :
ast.cmx :
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
javapm.cmo :
javapm.cmx :
parser.cmi : ast.cmo
