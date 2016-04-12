OBJS = ast.cmo parser.cmo scanner.cmo codegen.cmo plotter.cmo

TARFILES = Makefile  scanner.mll parser.mly \
	ast.ml codegen.ml plotter.ml

plotter : $(OBJS)
	ocamlc -o plotter $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean

clean :
	rm -f plotter parser.ml parser.mli scanner.ml *.svg testall.log \
	*.cmo *.cmi *.out *.diff

# 
ast.cmo: 
ast.cmx: 
bytecode.cmo: ast.cmo 
bytecode.cmx: ast.cmx 
compile.cmo:  ast.cmo 
compile.cmx:  ast.cmx 
plotter.cmo: scanner.cmo parser.cmi codegen.cmo \
    ast.cmo 
plotter.cmx: scanner.cmx parser.cmx codegen.cmx \
    bytecode.cmx ast.cmx 
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 