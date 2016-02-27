#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------
#------- CAUTION: any change in this directory should be also made in the
#------- twin Makefile in ../lib
include ../SETTINGS
include ../COMMON
include ../makefile-sysindep
#---------------------------------------------------------------------------
STDLIBDIR = $(REALLIBDIR)/contrib
ALLFILES = *
#---------------------------------------------------------------------------
# Local compilation of all libraries: since the CIAO compiler takes
# care of dependencies, an easy way is to compile a fake program which
# includes all loadable modules.  The library directory must be
# patched (hence -C -L) to point to the not-yet-installed libraries.
# We also make it to use the locally installed engine, since
# installation has not been made yet.
#---------------------------------------------------------------------------

alldyn:  message ciao_so_libs ciao_regular_libs autolibs

allstat: message ciao_regular_libs autolibs

message:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Building $(BASEMAIN) 'library' libraries..."
	@echo "*** (this may take a while and is silent; please be patient)"
	@echo "*** ---------------------------------------------------------"

ciao_so_libs:

ciao_regular_libs:

autolibs: 
	'rm' -f all_modules all_module_messages
	echo "set_prolog_flag(compresslib," $(COMPRESS_LIBS) ")." >> all_modules
	$(SRC)/etc/collect_modules all_modules
	$(SETLOCALCIAOSHELL) $(CIAOSHELL) < all_modules > all_module_messages
	'rm' -f all_module_messages

# .pos, etc, not made now; they are copied from the distribution
# 	$(SETLOCALCOMPILER); $(CIAOC) all_modules
# 	/bin/rm -f all_modules.itf all_modules.po all_modules.dep all_modules


# May copy far more than what is needed
install:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing $(BASEMAIN) 'library' libraries..."
	@echo "*** (in $(STDLIBDIR))"
	@echo "*** ---------------------------------------------------------"
	mkdir -p $(STDLIBDIR)
	-chmod $(EXECMODE) $(STDLIBDIR)
	-cp -pR $(ALLFILES) $(STDLIBDIR)
	(cd $(STDLIBDIR) && find . -type d -exec chmod $(EXECMODE) {} \; )
	(cd $(STDLIBDIR) && find . ! -type d -exec chmod $(DATAMODE) {} \; )
	(cd $(STDLIBDIR) && find . -name '*~' -exec 'rm' -f {} \; )

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
	find . -name '*.itf' -exec 'rm' -f {} \;
	find . -name '*.po'  -exec 'rm' -f {} \;
	find . -name '*.asr' -exec 'rm' -f {} \;

