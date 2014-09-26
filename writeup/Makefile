sharing.pdf: sharing.bib sharing.tex 
	pdflatex -interaction=nonstopmode sharing
	bibtex sharing
	pdflatex -interaction=nonstopmode sharing
	pdflatex -interaction=nonstopmode sharing


clean:
	rm *.aux *.log *.out *.bbl *.blg *.dvi *.aux *.fff *.lof *.out 