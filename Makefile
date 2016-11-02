OBJS = ast.cmo parser.cmo scanner.cmo javamp.cmo 

YACC = ocamlyacc

javamp: $(OBJS)
	ocamlc -o javamp $(OBJS)

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
	rm -f javamp parser.ml parser.mli scanner.ml \
	    *.cmo *.cmi *.out *.diff *.output javamp *.dSYM

.PHONY: all
all: clean javamp

ast.cmo :
ast.cmx :
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
javamp.cmo :
javamp.cmx :
parser.cmi : ast.cmo
