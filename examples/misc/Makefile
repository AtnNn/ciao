# Copyright (C) 1997, UPM-CLIP


suite:
	ciaoc suite

exec_suite: suite
	./suite

clean:
	/bin/rm -rf *.itf *.dep *.po 

realclean: clean
	/bin/rm -rf suite suite.cpx

distclean: realclean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up tests directory."
	@echo "*** ---------------------------------------------------------"
	/bin/rm -rf *~
