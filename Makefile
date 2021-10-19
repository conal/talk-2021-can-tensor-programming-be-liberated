TALK=tensor-liberated

# all: latex/$(PAPER).pdf
all: latex/$(TALK).pdf

LATEX_DEPENDENCIES:= \
  latex/macros.tex

PRECIOUS: $(LATEX_DEPENDENCIES) latex/$(TALK).tex

latex/%: %
	@mkdir -p $(dir $@)
	cp $< $@

latex/%.pdf: $(LATEX_DEPENDENCIES) latex/%.tex
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

