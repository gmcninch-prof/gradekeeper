

LIBDIR = /home/george/Code/lib
SCRIPTDIR = /home/george/Code/admin

all: script idris

.PHONY = script

script: gradekeeper
	cp $< $(SCRIPTDIR)

libtime_wrappers.so: $(wrapper_src)
	cc -shared $< -o $@
	cp $@ $(LIBDIR)

.PHONY = idris

idris:  
	pack build gradekeeper.ipkg
