OPT   := 1
DEBUG := 1

OCAMLFIND := ocamlfind

ifdef OPT
  OCAMLC := $(OCAMLFIND) ocamlopt 
  OCAMLDEP := $(OCAMLFIND) ocamldep -native
  OEXT := cmx
  AEXT := cmxa
else
  OCAMLC := $(OCAMLFIND) ocamlc
  OCAMLDEP := $(OCAMLFIND) ocamldep
  OEXT := cmo
  AEXT := cma
endif

LIBS := extlib unix glpk str

BUILD_FLAGS := $(LIBS:%=-package %)

ifdef DEBUG
  BUILD_FLAGS += -g
endif

ifndef OPT
  BUILD_FLAGS += -dllpath-all -dllpath delimcc
endif

ALINK_FLAGS := $(BUILD_FLAGS)
BUILD_FLAGS += -linkpkg
LINK_FLAGS := $(BUILD_FLAGS)

TARGETS := exp_price exp_pennies exp_pennies_variant exp_pennies_rnd exp_rock exp_rock_pennies exp_guess_my_number exp_guess_my_number2

all: $(TARGETS)

exp_%: util.$(OEXT) ppmonad.$(OEXT) exp_%.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS) $^ -o exp_$*

%.cmo : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmx : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmx %.cmxi : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmi : %.mli
	$(OCAMLC) $(BUILD_FLAGS) -c $<

clean:
	rm -Rf *.cmx *.cmi *.cmo *.o $(TARGETS) .dep

.dep: $(find *.ml *.mli)
	$(OCAMLDEP) *.ml *.mli > .dep

-include .dep
