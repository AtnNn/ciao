CIAOBUILDDIRtemp=../../../../../build
-include $(CIAOBUILDDIRtemp)/CONFIG_mkf

PROLOGSOURCES=*.pl

all: prolog

prolog:
	$(SETLOCALCIAO) $(CIAOC) -x $(PROLOGSOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f *.itf *.po *.dep *.asr

distclean: realclean
	/bin/rm -f *~ calculator



