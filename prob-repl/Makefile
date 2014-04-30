OCAMLFIND := ocamlfind

MLLIB := $(shell ocamlc -where)

OCAMLC := $(OCAMLFIND) ocamlopt
OCAMLLEX := ocamllex.opt
OCAMLYACC := ocamlyacc
OCAMLDEP := ocamldep -ml-synonym .mll -ml-synonym .mly -native
OEXT := cmx

#LIBS_GFX := glMLite glMLite.glut glMLite.glu sdl sdl.sdlimage sdl.sdlttf
LIBS_GFX := extlib sdl2.ba sdl2 sdl2_img glMLite glMLite.glut glMLite.glu 
LIBS_MULTICORE := netmulticore
LIBS_UTIL := core_extended lambda-term unix 
LIBS_ALL := $(LIBS_GFX) $(LIBS_MULTICORE) $(LIBS_UTIL)

BUILD_FLAGS_COMMON := -linkpkg -linkall -I $(MLLIB)/gmp gmp.cmxa -I /usr/local/lib/ppl ppl_ocaml.cmxa -thread -g
BUILD_FLAGS_ALL := $(BUILD_FLAGS_COMMON) $(LIBS_ALL:%=-package %)

LINK_PPL := -cclib -lppl -cclib -lm -cclib -lgmpxx -cclib -lgmp
LINK_GFX :=  -cclib "-framework CoreFoundation -framework Cocoa" $(LIBS_GFX:%=-package %)
LINK_MULTICORE := $(LIBS_MULTICORE:%=-package %)
LINK_UTIL := $(LIBS_UTIL:%=-package %)

LINK_FLAGS_REPL := $(BUILD_FLAGS_COMMON) $(LINK_PPL) $(LINK_GFX) $(LINK_MULTICORE) $(LINK_UTIL)

LINK_FLAGS_CINTERP := $(BUILD_FLAGS_COMMON) $(LINK_UTIL)
LINK_FLAGS_AINTERP := $(BUILD_FLAGS_COMMON) $(LINK_PPL) $(LINK_UTIL)

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
	$(OCAMLC) $(LINK_FLAGS_REPL) $^ -o repl

ainterp:\
 util.$(OEXT) \
 lang.$(OEXT) lexer.$(OEXT) parserutil.$(OEXT) parser.$(OEXT) \
 gmputil.$(OEXT) pplutil.$(OEXT) \
 eval.$(OEXT) stateconcrete.$(OEXT) preprocess.$(OEXT) \
 library.$(OEXT) librarylinear.$(OEXT)  stateabstract.$(OEXT) ppldomainbox.$(OEXT) \
 evalconcrete.$(OEXT) evalabstract.$(OEXT) \
 ainterp.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS_AINTERP) $^ -o ainterp

cinterp:\
 util.$(OEXT) \
 lang.$(OEXT) lexer.$(OEXT) parserutil.$(OEXT) parser.$(OEXT) \
 stateconcrete.$(OEXT) \
 library.$(OEXT) \
 preprocess.$(OEXT) eval.$(OEXT) stateconcrete.$(OEXT) evalconcrete.$(OEXT) \
 cinterp.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS_CINTERP) $^ -o cinterp

parsedebug: parser.mly
	$(OCAMLYACC) -v parser.mly

lexer.ml : parser.ml
lexer.$(OEXT) : parser.$(OEXT)
parser.$(OEXT) : parser.cmi
parser.cmi : lang.$(OEXT)

-include .dep

%.cmx %.cmxi : %.ml
	$(OCAMLC) $(BUILD_FLAGS_ALL) -c $<

%.cmi : %.mli
	$(OCAMLC) $(BUILD_FLAGS_ALL) -c $<

%.ml : %.mll
	$(OCAMLLEX) $<

%.ml %.mli : %.mly
	$(OCAMLYACC) $<

clean:
	rm -Rf *.cmx *.cmi *.cmo *.o repl cinterp ainterp netplex.* lexer.{cmi,cmx,ml} parser.{cmi,cmx,ml,mli} .dep parser.output

depend: .dep

.dep: $(find *.ml *.mli *.mly *.mll -not -iname lexer.ml -not -iname parser.ml)
	$(OCAMLDEP) *.ml *.mli > .dep
