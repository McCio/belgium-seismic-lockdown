all: draft.pdf draft open
.PHONY = all draft open uccs mems ostb

only_errors = 2>&1 1>/dev/null

project.uccs.pdf: project.uccs.Rmd project.Rmd
	R -e "rmarkdown::render('project.uccs.Rmd')"
uccs: project.uccs.pdf

project.mems.pdf: project.mems.Rmd project.Rmd
	R -e "rmarkdown::render('project.mems.Rmd')"
mems: project.mems.pdf

project.ostb.pdf: project.ostb.Rmd project.Rmd
	R -e "rmarkdown::render('project.ostb.Rmd')"
	#R -e "knitr::knit('project.ostb.Rmd')"
ostb: project.ostb.pdf

draft.pdf: draft.tex project.bib 
	-pdflatex -synctex=1 -interaction=nonstopmode draft.tex $(only_errors)
	bibtex draft $(only_errors)
	-pdflatex -synctex=1 -interaction=nonstopmode draft.tex $(only_errors)
	-pdflatex -synctex=1 -interaction=nonstopmode draft.tex $(only_errors)

draft: project.uccs.pdf project.mems.pdf draft.pdf

open: draft.pdf
	okular draft.pdf $(only_errors) &

