DOC = paper
DOC_FINAL = final
DEP = ${DOC}.tex defs.tex

doc: ${DEP}
	pdflatex -file-line-error ${DOC}

watch:
	perl watch.pl "make final" *.tex

first:
	pdflatex -file-line-error ${DOC}
	bibtex ${DOC}
	pdflatex -file-line-error ${DOC}
	pdflatex -file-line-error ${DOC}

docfinal:
	pdflatex -file-line-error -interaction batchmode ${DOC}

open:
	open ${DOC}.pdf &

bib:
	bibtex ${DOC}

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