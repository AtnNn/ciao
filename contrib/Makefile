#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------
#------- CAUTION: any change in this directory should be also made in the
#------- twin Makefile in ../lib
include ../SETTINGS
include ../SHARED
#---------------------------------------------------------------------------
STDLIBDIR = $(REALLIBDIR)/contrib
ALLFILES = *

all:  
	cd $(SRC)/contrib/chat80 && \
	$(SETLOCALCIAO) $(SRC)/etc/lpmake 
	'rm' -f all_modules all_module_messages
	-touch all_modules all_module_messages
	-chmod $(DATAMODE) all_modules all_module_messages
	echo "set_prolog_flag(compress_lib," $(COMPRESS_LIBS) ")." >> all_modules
	$(SRC)/etc/collect_modules all_modules
	$(SETLOCALCIAO) $(CIAOSHELL) < all_modules > all_module_messages
	'rm' -f all_module_messages

install: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing $(BASEMAIN) contributed libraries..."
	@echo "*** (in $(STDLIBDIR))"
	@echo "*** ---------------------------------------------------------"
	mkdir -p $(STDLIBDIR)
	-chmod $(EXECMODE) $(STDLIBDIR)
	-cp -pr $(ALLFILES) $(STDLIBDIR)
	-(cd $(STDLIBDIR) && find . -type d -exec chmod $(EXECMODE) {} \; )
	-(cd $(STDLIBDIR) && find . ! -type d -exec chmod $(DATAMODE) {} \; )
	-(cd $(STDLIBDIR) && find . -name '*~' -exec 'rm' -f {} \; )

uninstall:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Uninstalling $(BASEMAIN) 'library' libraries..."
	@echo "*** (deleting $(STDLIBDIR))"
	@echo "*** ---------------------------------------------------------"
	-rm -rf $(STDLIBDIR)

# *.itf *.po *.asr are eliminated recursively so that no Makefiles 
# underneath  are needed for cleaning up prolog-only libraries. MH.
distclean: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning $(STDLIBDIR)/library..."
	@echo "*** ---------------------------------------------------------"
	cd $(SRC)/contrib/chat80 && \
	$(SETLOCALCIAO) $(SRC)/etc/lpmake distclean
	find . -name '*.itf' -exec 'rm' -f {} \;
	find . -name '*.po'  -exec 'rm' -f {} \;
	find . -name '*.o' -exec 'rm' -f {} \;
	find . -name '*.so' -exec 'rm' -f {} \;
	find . -name '*.dll' -exec 'rm' -f {} \;
	find . -name '*_glue.c' -exec 'rm' -f {} \;
	find . -name '*.asr' -exec 'rm' -f {} \;
	find . -name '*.ass' -exec 'rm' -f {} \;
	find . -name '*.ast' -exec 'rm' -f {} \;

