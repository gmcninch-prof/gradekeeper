CONVERT=../assets/convert.js

PD=pandoc --standalone --from markdown  -V linkcolor:red --metadata-file=assets/metadata.yaml 

GK=../build/exec/gradekeeper

CSS_DEFAULT="assets/default.css"

VPATH = src:assets

json=Mathxyz-definitions.json

targets=$(wildcard *.md)
targets_html=$(targets:.md=.html)
targets_pdf=$(targets:.md=.pdf)



all: node convert scores html pdf

html: $(targets_html)

pdf: $(targets_pdf)

.PHONY = convert

.PHONY=node
node:
	npm install csvtojson

convert:
	@for j in $(json); do $(CONVERT) $$j; done

.PHONY = scores

scores:
	@for j in $(json); do $(GK) ./$$j; done


MJ=https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js

%.html: %.md  
	$(PD) $<  --css=$(CSS_DEFAULT) --mathjax=$(MJ) --to html  -o $@

%.pdf: %.md  
	$(PD) $<  --css=$(CSS_DEFAULT) --to pdf  -o $@


%.json: %.csv
	$(CONVERT) $<

.PHONY= echoes

echoes:
	@echo $(targets)
	@echo $(targets_pdf)
	@echo $(targets_html)
	@echo JSON: $(json)

.PHONY = clean
clean: clean_html clean_pdf clean_latex clean_links

clean_html: 
	-rm -f $(current_html) 

