TALK=tensor-liberated

# all: latex/$(PAPER).pdf
all: latex/$(TALK).pdf

LATEX_DEPENDENCIES:= \
  latex/macros.tex \
  latex/unicode.tex \

PRECIOUS: $(LATEX_DEPENDENCIES) latex/$(TALK).tex

latex/%.tex: %.lhs macros.tex formatting.fmt Makefile
	lhs2TeX -o $@ $<

latex/%: %
	@mkdir -p $(dir $@)
	cp $< $@

dots = $(wildcard Figures/*.dot)
pdfs = $(addsuffix .pdf, $(basename $(dots)))

# Cap the size so that LaTeX doesn't choke.
Figures/%.pdf: Figures/%.dot # Makefile
	dot -Tpdf -Gmargin=0 -Gsize=10,10 $< -o $@

pdfs: $(pdfs)

test:
	@echo $(pdfs)

latex/%.pdf: $(LATEX_DEPENDENCIES) latex/%.tex $(pdfs)
	cd latex && latexmk -xelatex -bibtex $*.tex
	@touch $@

# The touch is in case latexmk decides not to update the pdf.

SHOWPDF=skim

see: $(TALK).see

%.see: latex/%.pdf
	${SHOWPDF} $<

rmaux:
	rm latex/*.aux

clean:
	rm -r latex

web: .token

.token: latex/$(TALK).pdf
	scp $< conal@conal.net:/home/conal/domains/conal/htdocs/talks/tensor-liberated.pdf
	@touch $@

