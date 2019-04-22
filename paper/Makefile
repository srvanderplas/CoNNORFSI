# Makefile for knitr

# optionally put all RNW files to be compiled to pdf here, separated by spaces
RNW_FILES= $(wildcard *.Rnw)

# location of Rscript
R_HOME?=/usr/bin/Rscript

# these pdf's will be compiled from Rnw and Rmd files
PDFS= $(RNW_FILES:.Rnw=.pdf)

# these R files will be untangled from RNoWeb files
R_FILES= $(RNW_FILES:.Rnw=-purled.R)

# cache and figure directories
CACHEDIR= cache
FIGUREDIR= figures
IMAGEDIR= images


.PHONY: all purled clean cleanall open
.SUFFIXES: .Rnw .pdf .R .tex

# these targets will be made by default
all: $(PDFS)

# use this to create R files extracted from RNoWeb files
purled: $(R_FILES)

# these tex files will be generate from Rnw files
TEX_FILES = $(RNW_FILES:.Rnw=.tex)


# remove generated files except pdf and purled R files
clean:
	rm -f *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.fls
	rm -f $(TEX_FILES)

# run the clean target then remove pdf and purled R files too
cleanall: clean
	rm -f *.synctex.gz
	rm -f $(PDFS)
	rm -f $(R_FILES)

# compile a PDF from a RNoWeb file
%.pdf: %.Rnw Makefile
	mkdir -p build
	cp mybibfile.bib build/mybibfile.bib
	$(R_HOME)/bin/Rscript\
		-e "require(knitr)" \
		-e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
		-e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
		-e "knitr::knit('$*.Rnw', output='build/$*.tex')"
	cp -r $(IMAGEDIR) build/$(IMAGEDIR)
	latexmk -pdf -halt-on-error -output-directory=build build/$*.tex || cp build/$*.pdf $*.pdf

# extract an R file from an RNoWeb file
%-purled.R: %.Rnw
	$(R_HOME)/bin/Rscript\
		-e "require(knitr)" \
		-e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
		-e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
		-e "knitr::purl('$*.Rnw', '$*-purled.R')"

# open all PDF's
open:
	open -a Skim $(PDFS)