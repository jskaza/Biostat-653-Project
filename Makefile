main.pdf: bibliography.bib main.tex figures/meta.tex figures/attention.tex figures/meditation.tex figures/bivariate.tex figures/brain.png figures/device.png 
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
	R CMD BATCH render.R # generate corresponding slides

figures/%.tex: main.R
	R CMD BATCH main.R
