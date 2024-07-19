

LIBDIR = /home/george/Code/lib
SCRIPTDIR = /home/george/assets/admin-code
BINDIR = /home/george/.local/bin


all: scripts idris

.PHONY = scripts
scripts: 
	cp assets/gradekeeper $(SCRIPTDIR)
	cp assets/grade-data.scm $(BINDIR)

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
