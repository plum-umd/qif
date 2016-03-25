PDFLATEX := pdflatex -synctex=1
BIBTEX   := bibtex
OPEN     := open -a /Applications/Skim.app

DOC = belief-smc
DOC_FINAL = final
DEP = ${DOC}.tex biblio.bib

doc: ${DEP}
	$(PDFLATEX) -file-line-error ${DOC}

watch:
	perl watch.pl "make final" *.tex *.bib

first:
	$(PDFLATEX) -file-line-error ${DOC}
	bibtex ${DOC}
	$(PDFLATEX) -file-line-error ${DOC}
	$(PDFLATEX) -file-line-error ${DOC}

docfinal:
	$(PDFLATEX) -file-line-error -interaction batchmode ${DOC}

open:
	$(OPEN) ${DOC}.pdf &

bib:
	$(BIBTEX) ${DOC}

final:
	make clean
	make docfinal
	make bib
	make docfinal
	make docfinal
	make docfinal
	make docfinal
	cp ${DOC}.pdf ${DOC_FINAL}.pdf

cleandoc:
	rm -Rf ${DOC}.pdf

clean:
	rm -Rf *.aux *.lof *.log *.lot *.bbl *.blg *.toc; true
	rm -Rf *~; true
	rm -Rf .DS_Store; true
	rm -Rf .iTeXMac; true

cleanfinal:
	make clean
	rm -Rf ${DOC_FINAL}.pdf
