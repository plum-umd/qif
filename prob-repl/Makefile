OCAMLFIND := ocamlfind

OCAMLC := $(OCAMLFIND) ocamlc
OCAMLLEX := ocamllex
OCAMLYACC := ocamlyacc
OCAMLDEP := ocamldep -ml-synonym .mll -ml-synonym .mly -native
OEXT := cmo
LOEXT := cma

LIBS := extlib \
	glMLite glMLite.glut glMLite.glu \
	sdl2.ba sdl2 sdl2_img \
        netmulticore \
	core_extended lambda-term unix gmp 

BUILD_FLAGS := -linkpkg \
	$(LIBS:%=-package %) \
	-I /usr/local/lib/ppl ppl_ocaml.$(LOEXT) -thread -g \


LINK_FLAGS := $(BUILD_FLAGS) -thread -linkpkg \
	-cclib -lppl -cclib -lgl_stubs \
	-cclib "-framework OpenGL -framework GLUT -framework CoreFoundation -framework Cocoa" \
	$(LIBS:%=-package %)

repl:\
 util.$(OEXT) config.$(OEXT) \
 lang.$(OEXT) lexer.$(OEXT) parserutil.$(OEXT) parser.$(OEXT) \
 gmputil.$(OEXT) pplutil.$(OEXT) \
 ppldomainbox.$(OEXT) \
 eval.$(OEXT) preprocess.$(OEXT) stateconcrete.$(OEXT) library.$(OEXT) evalconcrete.$(OEXT) \
 librarylinear.$(OEXT) stateabstract.$(OEXT) evalabstract.$(OEXT) \
 ipc.$(OEXT) worker.$(OEXT) cmd.$(OEXT) master.$(OEXT) \
 sdldefs.$(OEXT) glutil.$(OEXT) uigl.$(OEXT) ui.$(OEXT) \
 repl.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS) $^ -o repl

ainterp:\
 util.$(OEXT) \
 lang.$(OEXT) lexer.$(OEXT) parserutil.$(OEXT) parser.$(OEXT) \
 gmputil.$(OEXT) pplutil.$(OEXT) \
 eval.$(OEXT) stateconcrete.$(OEXT) preprocess.$(OEXT) \
 library.$(OEXT) librarylinear.$(OEXT)  stateabstract.$(OEXT) ppldomainbox.$(OEXT) \
 evalconcrete.$(OEXT) evalabstract.$(OEXT) \
 ainterp.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS) $^ -o ainterp

cinterp:\
 util.$(OEXT) \
 lang.$(OEXT) lexer.$(OEXT) parserutil.$(OEXT) parser.$(OEXT) \
 stateconcrete.$(OEXT) \
 library.$(OEXT) \
 preprocess.$(OEXT) eval.$(OEXT) stateconcrete.$(OEXT) evalconcrete.$(OEXT) \
 cinterp.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS) $^ -o cinterp

parsedebug: parser.mly
	$(OCAMLYACC) -v parser.mly

#lexer.ml : parser.ml
lexer.$(OEXT) : parser.$(OEXT)
parser.$(OEXT) : parser.cmi lang.$(OEXT)
#parser.cmi : lang.$(OEXT) lang.$(OEXT)

stateabstract.cmo: ppldomain.mli

-include .dep

%.cmi : %.mli
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmx %.cmxi : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmo %.cmi : %.ml
	$(OCAMLC) $(BUILD_FLAGS) $(filter %.mli,$^) -c $<

#%.cmi : %.mli
#	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.ml : %.mll
	$(OCAMLLEX) $<

%.ml %.mli : %.mly
	$(OCAMLYACC) $<

clean:
	rm -Rf *.cmx *.cmi *.cmo *.o repl cinterp ainterp netplex.* lexer.{cmi,cmx,ml} parser.{cmi,cmx,ml,mli} .dep parser.output

depend: .dep

.dep: $(find *.ml *.mli *.mly *.mll -not -iname lexer.ml -not -iname parser.ml)
	$(OCAMLDEP) *.ml *.mli > .dep
