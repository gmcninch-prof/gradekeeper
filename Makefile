
LIBDIR = /home/george/Code/lib
INSTALLDIR = $(HOME)/.local/bin

all: idris

install: $INSTALLDIR/grade-data.scm $INSTALLDIR/gradekeeper


$INSTALLDIR/grade-data.scm: src-guile/grade-data.scm
	cp src-guile/grade-data.scm $(INSTALLDIR)

$INSTALLDIR/gradekeeper: assets/gradekeeper
	cp assets/gradekeeper $(INSTALLDIR)

libtime_wrappers.so: $(wrapper_src)
	cc -shared $< -o $@
	cp $@ $(LIBDIR)

.PHONY = idris
idris:  
	pack build gradekeeper.ipkg


.PHONY = example
examp:
	echo "running make in 'example' directory"
	$(MAKE) -C example
