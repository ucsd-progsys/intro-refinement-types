

##############################################
TALK=120
##############################################
SRC=src-$(TALK)
##############################################

# MATHJAX=http://cdn.mathjax.org/mathjax/latest
MATHJAX=js/MathJax-2.6.0
LIQUIDCLIENT=./liquid-client
SLIDES=dist/_slides
SITE=dist/_site
DIST=dist/_build
TEMPLATES=assets/templates
FILTERS=assets/filters
JS=assets/js
CSS=assets/css
IMG=assets/img
GHPAGE=../intro-refinement-types-pages/

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

LIQUID=liquid --short-names
####################################################################

lhsObjects   := $(wildcard $(SRC)/*.lhs)
texObjects   := $(patsubst %.lhs,%.tex,$(wildcard $(SRC)/*.lhs))
htmlObjects  := $(patsubst %.lhs,%.html,$(wildcard $(SRC)/*.lhs))
mdObjects    := $(patsubst %.lhs,%.lhs.markdown,$(wildcard $(SRC)/*.lhs))
slideObjects := $(patsubst %.lhs,%.lhs.slide.html,$(wildcard $(SRC)/*.lhs))

####################################################################

all: html

################ rust style html ###################################

html: indexhtml $(htmlObjects)
	cp $(SRC)/*.html            $(SITE)/
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
	$(INDEXER) $(SRC)/ $(METATEMPLATE) $(INDEXTEMPLATE) $(PAGETEMPLATE) $(INDEX) $(LINKS)

$(SRC)/%.html: $(SRC)/%.lhs
	PANDOC_TARGET=$@ PANDOC_CODETEMPLATE=$(LIQUIDCLIENT)/templates/code.template $(PANDOCHTML) --template=$(PAGETEMPLATE) $(PREAMBLE) $? $(TEMPLATES)/bib.lhs -o $@

################ reveal slides html ###################################

slides: $(slideObjects)
	mv $(SRC)/*.html $(SLIDES)/
	cp -r $(IMG)  $(SLIDES)/
	cp -r $(JS)   $(SLIDES)/
	cp -r $(CSS)  $(SLIDES)/


$(SRC)/.liquid/%.lhs.markdown: $(SRC)/%.lhs
	-$(LIQUID) $?

$(SRC)/%.lhs.slide.html: $(SRC)/.liquid/%.lhs.markdown
	$(REVEAL) $? -o $@

################ CLEAN and SYNC #######################################

clean:
	rm -rf $(DIST)/* && rm -rf $(SRC)/*.tex && rm -rf $(SRC)/.liquid && rm -rf $(SRC)/*.html

distclean:
	rm -rf $(DIST)/* && rm -rf $(SITE)/* && rm -rf $(SRC)/*.tex && rm -rf $(SRC)/.liquid && rm -rf $(SRC)/*.html

upload:
	cp -r $(SITE)/* docs/$(TALK)/
	cd docs/ && git add . && git commit -a -m "update page" && git push origin master

# upload: all
#	cp -r $(SITE)/* $(GHPAGE)
#	cd $(GHPAGE) && git add . && git commit -a -m "update page" && git push origin gh-pages


#clean:
#	cd lhs/ && ../cleanup && cd ../
#	cd html/ && rm -rf * && cd ../
#	cp index.html html/
