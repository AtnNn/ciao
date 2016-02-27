# Copyright (C) 1997, UPM-CLIP

include ../SETTINGS
include ../COMMON
include ../makefile-sysindep

suite:
	$(SETLOCALCOMPILER) $(CIAOC) suite

exec_suite:
	$(SETLOCALCOMPILER) ./suite

clean:
	/bin/rm -rf *.itf *.dep *.po 

realclean: clean
	/bin/rm -rf suite

distclean: realclean
	/bin/rm -rf *~
