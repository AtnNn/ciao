#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------
#------- CAUTION: any change in this directory should be also made in the 
#------- twin Makefile in ../library 
include ../SETTINGS
include ../COMMON
include ../makefile-sysindep
#---------------------------------------------------------------------------
STDLIBDIR = $(REALLIBDIR)/lib
ALLFILES = *
#---------------------------------------------------------------------------
# Local compilation of all libraries: since the CIAO compiler takes
# care of dependencies, an easy way is to compile a fake program which
# includes all loadable modules.  The library directory must be
# patched (hence -C -L) to point to the not-yet-installed libraries.
# We also make it to use the locally installed engine, since
# installation has not been made yet.
#---------------------------------------------------------------------------

all: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Building $(BASEMAIN) 'lib' libraries..."
	@echo "*** (this may take a while and is silent; please be patient)"
	@echo "*** ---------------------------------------------------------"
	$(MAKE) autolibs

autolibs: 
	$(SRC)/etc/generate_foreign_compile_options $(MAKE) $(SRC)/makefile-sysdep $(SRC)/lib/auto_compile_options.pl
	-chmod $(DATAMODE) $(SRC)/lib/auto_compile_options.pl
	'rm' -f all_modules all_module_messages
	$(SRC)/etc/collect_modules all_modules
	$(SETLOCALCIAOSHELL) $(CIAOSHELL) < all_modules > all_module_messages
	-chmod $(DATAMODE) all_modules
	'rm' -f all_module_messages

# May copy far more than what is needed
install:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing $(BASEMAIN) 'lib' libraries..."
	@echo "*** (in $(STDLIBDIR))"
	@echo "*** ---------------------------------------------------------"
	mkdir -p $(STDLIBDIR)
	-chmod $(EXECMODE) $(STDLIBDIR)
	-cp -pR $(ALLFILES) $(STDLIBDIR)
	(cd $(STDLIBDIR); find . -type d -exec chmod $(EXECMODE) {} \; )
	(cd $(STDLIBDIR); find . ! -type d -exec chmod $(DATAMODE) {} \; )
	(cd $(STDLIBDIR); find . -name '*~' -exec 'rm' -f {} \; )

uninstall:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Uninstalling $(BASEMAIN) 'lib' libraries..."
	@echo "*** (deleting $(STDLIBDIR))"
	@echo "*** ---------------------------------------------------------"
	-rm -rf $(STDLIBDIR)

# *.itf *.po *.asr are eliminated recursively so that no Makefiles 
# underneath  are needed for cleaning up prolog-only libraries. MH.
distclean: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning $(BASEMAIN)/lib..."
	@echo "*** ---------------------------------------------------------"
	find . -name '*.itf' -exec 'rm' -f {} \;
	find . -name '*.po'  -exec 'rm' -f {} \;
	find . -name '*.asr' -exec 'rm' -f {} \;
