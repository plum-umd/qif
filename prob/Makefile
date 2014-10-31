DEBUG_FLAGS = -g
#DEBUG_FLAGS = 
CDEBUG_FLAGS = -g
#CDEBUG_FLAGS =

#USE_OPT = 1

OCAMLYACC := ocamlyacc
OCAMLLEX := ocamllex
OCAMLFIND := ocamlfind

ifdef USE_OPT
  OEXT := cmx
  IEXT := cmxi
  LOEXT := cmxa
  OCAMLC := $(OCAMLFIND) ocamlc.opt
  OCAMLDEP := $(OCAMLFIND) ocamldep.opt -native
else
  OEXT := cmo
  IEXT := cmi
  LOEXT := cma
  OCAMLC := $(OCAMLFIND) ocamlc
  OCAMLDEP := $(OCAMLFIND) ocamldep
endif

SOURCES_COMMON :=\
	util.ml \
	benchmark.ml \
	globals.ml \
	lang.ml \
	logical.ml \
	pdefs.ml smcdefs.ml \
	bimap.ml \
	numeric.ml \
	gmp_util.ml \
	geo.ml \
	ppl_util.ml \
	maths.ml \
	err.ml \
	state.ml \
	latte.ml \
	ppldomainpoly.ml \
	ppldomainbox.ml \
	ppldomainocta.ml \
	ppldomainoctalatte.ml \
	pplstatesetmaker.ml \
	pstatesetmaker.ml \
	evalstate.ml  \
	ppowersetmaker.ml \
	evalpstateset.ml \
	esys.ml \
	lexer.ml parser_util.ml parser.ml \
	preeval.ml \
	policy.ml 

SOURCES_LATEX := \
	$(SOURCES_COMMON) \
	latex.ml

SOURCES_PROB := \
	$(SOURCES_COMMON) \
	prob.ml

SOURCES_MPROB := \
	$(SOURCES_COMMON) \
	smcdefs.ml \
	disjunctive.ml \
	mprob.ml

SOURCES_ALL := $(SOURCES_PROB) $(SOURCES_CEVAL) $(SOURCES_MPROB)

OBJS_PROB  := $(SOURCES_PROB:.ml=.$(OEXT))
OBJS_MPROB := $(SOURCES_MPROB:.ml=.$(OEXT))
OBJS_LATEX := $(SOURCES_LATEX:.ml=.$(OEXT))

OBJS_ALL   := $(OBJS_PROB) $(OBJS_CEVAL)

COPTS = $(CDEBUG_FLAGS) -w

PPL_INC = -I /usr/local/lib/ppl 
PPL_LINK = $(PPL_INC) -cc g++ -cclib -lppl $(COPTS:%=-ccopt %) ppl_ocaml.$(LOEXT)
INC_FLAGS = -package str -package unix -package gmp
LINK_FLAGS = $(DEBUG_FLAGS) $(INC_FLAGS) -linkpkg
BUILD_FLAGS = $(DEBUG_FLAGS) $(INC_FLAGS) -pp "camlp5o ./pa_if.cmo" -I +camlp5

prob: $(OBJS_PROB) latte_tmp
	$(OCAMLC) $(PPL_LINK) $(LINK_FLAGS) -o prob $(OBJS_PROB)

mprob: $(OBJS_MPROB) 
	$(OCAMLC) $(PPL_LINK) $(LINK_FLAGS) -o mprob $(OBJS_MPROB)

pa_if.cmo: pa_if.ml
	$(OCAMLC) -package camlp5 -c -pp camlp5r $^ -o pa_if.cmo

latex: $(OBJS_LATEX)
	$(OCAMLC) $(PPL_LINK) $(LINK_FLAGS) -o latex $^

util.ml: pa_if.cmo

latte_tmp:
	mkdir latte_tmp

-include .dep

clean_latte:
	rm -Rf *.lp *.lps numOfLatticePoints totalTime numOfUnimodularCones latte_stats Check_emp.ddl.old latte_tmp/* tri.* *.ddl *.out *.cdd *.ead *.ext
	rm -Rf latte_cdd.* Check_emp.* cdd.error

clean_build:
	rm -rf *~ *.cm* *.o lexer.ml parser.ml parser.mli parser.output *.dSYM .dep .DS_Store 

clean:
	make clean_latte
	make clean_build
	rm -Rf prob mprob latex

.dep: lexer.ml parser.mli parser.ml

%.ml : %.mll
	$(OCAMLLEX) $<

%.ml %.mli : %.mly
	$(OCAMLYACC) $<

%.cmi : %.mli
	$(OCAMLC) -c $(BUILD_FLAGS) $(PPL_INC) -o $@ $<

%.cmx:	%.ml 
	$(OCAMLC) -c $(BUILD_FLAGS) $(PPL_INC) -o $@ $<

%.cmo :	%.ml 
	$(OCAMLC) -c $(BUILD_FLAGS) $(PPL_INC) -o $@ $<

SRC_ALL := 

.dep: pa_if.cmo

.dep: $(filter-out pa_if.ml,$(wildcard *.ml)) $(wildcard *.mli)
	$(OCAMLDEP) -pp "camlp5o ./pa_if.cmo" $^ > .dep


