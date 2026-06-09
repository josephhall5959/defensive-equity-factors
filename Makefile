# ============================================================================
# Makefile — reproduce the analysis and build the paper.
#   make all      run the R pipeline and compile the PDF (default)
#   make pipeline run the R data/analysis pipeline only
#   make paper    compile paper/paper.pdf from existing outputs
#   make clean    remove LaTeX aux files and rendered outputs
#   make distclean also remove downloaded raw data
# ============================================================================

R       := Rscript
PAPERDIR:= paper
TEX     := paper
RAW     := $(wildcard data/raw/*)

.PHONY: all pipeline paper clean distclean

all: pipeline paper

pipeline:
	$(R) run_all.R

paper: $(PAPERDIR)/$(TEX).pdf

$(PAPERDIR)/$(TEX).pdf: $(PAPERDIR)/$(TEX).tex $(PAPERDIR)/references.bib \
                        output/tables/numbers.tex
	cd $(PAPERDIR) && pdflatex -interaction=nonstopmode -halt-on-error $(TEX).tex
	cd $(PAPERDIR) && bibtex $(TEX)
	cd $(PAPERDIR) && pdflatex -interaction=nonstopmode -halt-on-error $(TEX).tex
	cd $(PAPERDIR) && pdflatex -interaction=nonstopmode -halt-on-error $(TEX).tex
	cd $(PAPERDIR) && pdflatex -interaction=nonstopmode -halt-on-error $(TEX).tex

clean:
	rm -f $(PAPERDIR)/*.aux $(PAPERDIR)/*.log $(PAPERDIR)/*.bbl \
	      $(PAPERDIR)/*.blg $(PAPERDIR)/*.out
	rm -f output/tables/*.tex output/figures/*.pdf
	rm -f data/processed/*

distclean: clean
	rm -f data/raw/*
