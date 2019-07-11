# MATHJAX=http://cdn.mathjax.org/mathjax/latest
MATHJAX=js/MathJax-2.6.0
LIQUIDCLIENT=../liquid-client
SLIDES=dist/_slides
SITE=dist/_site
DIST=dist/_build
TEMPLATES=assets/templates
FILTERS=assets/filters
JS=assets/js
CSS=assets/css
IMG=assets/img
GHPAGE=../live/

##############################################
PANDOC=pandoc
INDEXER=$(FILTERS)/Toc.hs
METATEMPLATE=$(TEMPLATES)/pagemeta.template
INDEXTEMPLATE=$(TEMPLATES)/index.template
PREAMBLE=$(TEMPLATES)/preamble.lhs
BIB=$(TEMPLATES)/bib.lhs

# generated
PAGETEMPLATE=$(DIST)/page.template
LINKS=$(DIST)/links.txt
INDEX=$(DIST)/index.lhs

##############################################

PANDOCPDF=pandoc \
	--highlight-style=tango \
	--from=markdown+lhs \
	--biblio templates/sw.bib \
	--chapters \
	--latex-engine=pdflatex \
	--template=assets/templates/default.latex \
	--filter assets/filters/Figures.hs \
	--filter assets/filters/Latex.hs

##############################################

PANDOCHTML=$(PANDOC)\
     --from=markdown+lhs \
	 --to=html5 \
     -s --mathjax \
	 --standalone \
     --parse-raw \
	 --mathjax \
	 --toc \
	 --section-divs \
	 --filter $(LIQUIDCLIENT)/templates/codeblock.hs \
	 --filter $(FILTERS)/Figures.hs \
	 --filter $(FILTERS)/Html.hs \
	 --filter $(FILTERS)/HeaderSlides.hs \
	 --variable=notitle \
	 --highlight-style=tango

REVEAL=$(PANDOC)\
	   --from=markdown\
	   --to=html5                           \
	   --standalone                         \
	   --mathjax \
	   --section-divs                       \
	 --filter $(FILTERS)/Slides.hs \
	   --template=$(TEMPLATES)/template.reveal  \
	   --variable reveal=js/reveal.js \
	   --variable mathjax=$(MATHJAX)

# LIQUID=liquid --short-names

####################################################################

lhsObjects   := $(wildcard src/*.lhs)
texObjects   := $(patsubst %.lhs,%.tex,$(wildcard src/*.lhs))
htmlObjects  := $(patsubst %.lhs,%.html,$(wildcard src/*.lhs))
mdObjects    := $(patsubst %.lhs,%.lhs.markdown,$(wildcard src/*.lhs))
slideObjects := $(patsubst %.lhs,%.lhs.slide.html,$(wildcard src/*.lhs))

####################################################################

all: html

################ rust style html ###################################

pdf: $(lhsObjects)
	cat $(lhsObjects) > dist/pbook.lhs
	PANDOC_TARGET=pbook.pdf $(PANDOCPDF) $(PREAMBLE) $(BIB) dist/pbook.lhs -o dist/pbook.pdf

html: indexhtml $(htmlObjects)
	cp src/*.html               $(SITE)/
	cp -r $(IMG)                $(SITE)/
	cp -r $(CSS)                $(SITE)/

client: indexhtml $(htmlObjects)
	cp src/*.html               $(SITE)/
	cp -r $(IMG)                $(SITE)/
	cp -r $(CSS)                $(SITE)/
	cp -r $(LIQUIDCLIENT)/fonts $(SITE)/
	cp -r $(LIQUIDCLIENT)/css   $(SITE)/
	cp -r $(LIQUIDCLIENT)/js    $(SITE)/

mathjax:
	cp -r $(JS)                 $(SITE)/

indexhtml: $(INDEX)
	$(PANDOC) --from=markdown+lhs --to=html5 --template=$(INDEX) $(PREAMBLE) -o $(SITE)/index.html

$(INDEX):
	$(INDEXER) src/ $(METATEMPLATE) $(INDEXTEMPLATE) $(PAGETEMPLATE) $(INDEX) $(LINKS)

src/%.html: src/%.lhs
	PANDOC_TARGET=$@ PANDOC_CODETEMPLATE=$(LIQUIDCLIENT)/templates/code.template $(PANDOCHTML) --template=$(PAGETEMPLATE) $(PREAMBLE) $? $(TEMPLATES)/bib.lhs -o $@

################ reveal slides html ###################################

slides: $(slideObjects)
	mv src/*.html $(SLIDES)/
	cp -r $(IMG)  $(SLIDES)/
	cp -r $(JS)   $(SLIDES)/
	cp -r $(CSS)  $(SLIDES)/


src/.liquid/%.lhs.markdown: src/%.lhs
	-$(LIQUID) $?

src/%.lhs.slide.html: src/.liquid/%.lhs.markdown
	$(REVEAL) $? -o $@

################ CLEAN and SYNC #######################################

clean:
	rm -rf $(DIST)/* && rm -rf src/*.tex && rm -rf src/.liquid && rm -rf src/*.html


distclean:
	rm -rf $(DIST)/* && rm -rf $(SITE)/* && rm -rf src/*.tex && rm -rf src/.liquid && rm -rf src/*.html

upload: all
	cp -r $(SITE)/* $(GHPAGE)
	cd $(GHPAGE) && git add . && git commit -a -m "update page" && git push origin gh-pages

live: all
	cp -r $(SITE)/* $(GHPAGE)
	cd $(GHPAGE) && git add . && git commit -a -m "update page" && git push origin master



#clean:
#	cd lhs/ && ../cleanup && cd ../
#	cd html/ && rm -rf * && cd ../
#	cp index.html html/
