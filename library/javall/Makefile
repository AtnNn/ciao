include ../../SETTINGS
include ../../SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

PROLOGSOURCES=javasock.pl javart.pl jtopl.pl

all: 
	$(SETLOCALCIAO) $(CIAOC) -c $(PROLOGSOURCES)
	cd CiaoJava && $(MAKE) all
	cd examples/java2pl && $(MAKE) all
	cd examples/pl2java && $(MAKE) all
	cd javadoc && $(MAKE) all

clean realclean:
	/bin/rm -f *.itf *.po *.dep *.asr
	cd CiaoJava && $(MAKE) clean
	cd examples/java2pl && $(MAKE) clean
	cd examples/pl2java && $(MAKE) clean
	cd javadoc && $(MAKE) clean

distclean: realclean
	/bin/rm -f *~
	cd CiaoJava && $(MAKE) distclean
	cd examples/java2pl && $(MAKE) distclean
	cd examples/pl2java && $(MAKE) distclean
	cd javadoc && $(MAKE) distclean




