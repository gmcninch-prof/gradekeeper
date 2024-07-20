CONVERT=/home/george/.local/bin/grade-data.scm
PD=pandoc --standalone --from markdown  -V linkcolor:red --metadata-file=assets/metadata.yaml 
GK=gradekeeper
CSS_DEFAULT="assets/default.css"


MJ=https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js
# MJ=http://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML

#--------------------------------------------------------------------------------

ENROLL_CSV=Mathxyz-enrollment.csv
CANVAS_CSV=Mathxyz-canvas.csv

COURSE_SPEC=Mathxyz-spec.json
COURSE_DATA=Mathxyz-data.json

targets_md=Mathxyz-2023-2024-Fall.md
targets_html=$(targets_md:.md=.html)
targets_pdf=$(targets_md:.md=.pdf)


all: convert scores html pdf

convert: $(COURSE_DATA)
scores: $(targets_md)
html: $(targets_html)
pdf: $(targets_pdf)

$(COURSE_DATA): $(COURSE_SPEC) $(ENROLL_CSV) $(CANVAS_CSV)
	@unix2dos -q $(ENROLL_CSV)
	@unix2dos -q $(CANVAS_CSV)
	$(CONVERT) --enroll $(ENROLL_CSV) --course $(COURSE_SPEC) --canvas $(CANVAS_CSV) --output $(COURSE_DATA)


$(targets_md): $(COURSE_SPEC) $(COURSE_DATA)
	$(GK) --spec $(COURSE_SPEC) --data $(COURSE_DATA)


html: $(targets_html)

pdf: $(targets_pdf)

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

