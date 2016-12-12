main.pdf: bibliography.bib main.tex figures/meta.tex figures/attention.tex figures/meditation.tex figures/bivariate.tex figures/brain.png figures/device.png 
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
	rm *.aux
	rm *.blg
	rm *.log

figures/%.tex: main.R
	R CMD BATCH render.R

main.tex: 
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
	rm *.aux
	rm *.blg
	rm *.log
