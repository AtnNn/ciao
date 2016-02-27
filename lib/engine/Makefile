include ../../SETTINGS
include ../../SHARED

phony:
	@echo
	@echo 'What should I make?'
	@echo

exports:
	$(SETLOCALCIAO) $(CIAOC) -x $(SRC)/lib/compiler/browse_builtins.pl
	$(SETLOCALCIAO) $(SRC)/lib/compiler/browse_builtins
	/bin/rm -f ../../shell/ciaoshcope.po
	cd ../../ciaoc && $(MAKE)
	cd ../../shell && $(MAKE)


distclean:
	/bin/rm -rf *.dep *.po *.asr
	/bin/rm -rf TAGS
