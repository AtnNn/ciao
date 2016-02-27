# Copyright (C) 1997, UPM-CLIP

# This Makefile *needs* GNU make

# make all              compile the whole Ciao system (engine, libraries, docs)
# make install          install the whole Ciao system (engine, libraries, docs)
#
# make eng              compile the Ciao engine for this particular arch.
#			This is the only make action needed for using Ciao
#                       executables in several architectures at once.
# make cleanbackups     delete backup files
# make distclean        delete all files which can be automatically generated
# make engclean		delete all engines created
# make totalclean       cleanbackups + distclean
#
# These, condemned to disappear
#
# make clean		to delete object files etc. not used by ciao
# make realclean	to delete everything that wasn't in the distribution

#------- You should not change ANYTHING in this file -------------
#------- All customizable options are in the file SETTINGS -------

include SETTINGS
include COMMON

include $(SRC)/makefile-sysindep
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)
MFLAGS=-j$(PROCESSORS)

default: all$(ALT)

all:
	@echo "*** ========================================================="
	@echo "*** Compiling ciao"
	@echo "*** ========================================================="
	$(MAKE) eng compiler applications libraries
	@echo "*** ========================================================="
	@echo "*** Ciao compilation completed"
	@echo "*** ========================================================="

allwin32: engwin32 compiler applications libraries

allpl: compiler applications libraries

engin: eng$(ALT)

eng: bin/$(CIAOARCH)$(CIAODEBUG) include/$(CIAOARCH)$(CIAODEBUG) $(DEFAULTYPE)eng exe_header

engwin32: copysrcfiles eng
	rm -f $(SRC)/Win32/bin/$(ENGINENAME)
	cp $(OBJDIR)/$(ENGINENAME) $(SRC)/Win32/bin

dyneng: commoneng
	(umask 002; cd $(OBJDIR);  \
	 $(MAKE) configure.h; \
	 $(MAKE) $(MFLAGS) $(ENGINENAME) CURRLIBS='$(LIBS)')

stateng: commoneng
	(umask 002; cd $(OBJDIR);  \
	 $(MAKE) configure.h; \
	 $(MAKE) $(MFLAGS) $(ENGINENAME) ADDOBJ='$(STATOBJ)' \
		                         CURRLIBS='$(LIBS) $(STAT_LIBS)')

commoneng:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Compiling $(BASEMAIN) engine for $(OSNAME)/$(ARCHNAME)..."
	@echo "*** ---------------------------------------------------------"
	$(MAKE) $(MFLAGS) version-ciao

allengs:
	@for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Making engine in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(SRC); gmake eng" | rsh $$machine csh; \
		echo "setenv CIAODEBUG -debug; cd $(SRC); gmake eng" | rsh $$machine csh; \
	done

installallengs:
	@for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Making engine in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(SRC); gmake installeng" | rsh $$machine csh; \
	done

# Win32 header does not change
exe_header:
ifneq ($(OSNAME),Win32)
	cd lib/compiler; $(MAKE) exe_header
endif

compiler:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Compiling $(BASEMAIN) standalone compiler"
	@echo "*** (this may take a while and is silent; please be patient)"
	@echo "*** ---------------------------------------------------------"
	cd ciaoc; $(MAKE) compiler

applications:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Compiling $(BASEMAIN) toplevel shell & script interpreter ..."
	@echo "*** (this may take a while and is silent; please be patient)"
	@echo "*** ---------------------------------------------------------"
	cd shell; $(MAKE) all
	cd etc; $(MAKE) all

libraries:
	cd lib; $(MAKE) all
	cd library; $(MAKE) all$(DEFAULTYPE)

copysrcfiles: createsrcdir
	cd engine ; for File in *.[ch] Makefile ; \
	do if [ ! -f $(OBJDIR)/$${File} -o $${File} -nt $(OBJDIR)/$${File} ]; \
             then rm -f $(OBJDIR)/$${File} ; cp $${File} $(OBJDIR)/$${File} ; \
	   fi ; \
	done ;

include/$(CIAOARCH)$(CIAODEBUG):
	$(MAKE) createincludedir

bin/$(CIAOARCH)$(CIAODEBUG):
	$(MAKE) createsrcdir
	cd $(OBJDIR);	                   \
	   ln -s ../../engine/*.[ch] . ;   \
	   ln -s ../../engine/Makefile . ; \
	   rm -f configure.h

createsrcdir:
	if test ! -d $(SRC)/bin ; then \
	  mkdir $(SRC)/bin ; \
	  touch $(SRC)/bin/.nodistribute ; \
	  chmod $(DATAMODE) $(SRC)/bin/.nodistribute ; \
        fi
	if test ! -d $(OBJDIR) ; then \
	  mkdir $(OBJDIR) ; chmod ug+rwX $(OBJDIR) ; fi

createincludedir:
	if test ! -d $(SRC)/include ; then \
	  mkdir $(SRC)/include ; touch $(SRC)/include/.nodistribute ; fi
	if test ! -d $(SRCINCLUDEDIR) ; then \
	  mkdir $(SRCINCLUDEDIR) ; chmod ug+rwX $(SRCINCLUDEDIR) ; fi

version-ciao:
	-rm -f $(OBJDIR)/version.c
	@(umask 002; \
	echo 'char *emulator_version = "Ciao-Prolog '$(VERSION)' #'$(PATCH)': '`date`'";' >  $(OBJDIR)/version.c; \
	echo 'char *emulator_architecture = "$(ARCHNAME)";' >> $(OBJDIR)/version.c;\
	echo 'char *emulator_os = "$(OSNAME)";' >> $(OBJDIR)/version.c;\
	echo 'char *installibdir = "$(REALLIBDIR)";' >> $(OBJDIR)/version.c )

installeng: eng justinstalleng

justinstalleng:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing $(BASEMAIN) engine for $(OSNAME)/$(ARCHNAME)..."
	@echo "*** ---------------------------------------------------------"
	-(umask 002; mkdir -p $(REALLIBDIR); \
	cd $(OBJDIR); $(MAKE) install LD=$(LD) \
	CC=$(CC) CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' LIBS=$(LIBS))

uninstalleng:
#	@echo "*** ---------------------------------------------------------"
#	@echo "*** Uninstalling $(BASEMAIN) engine for $(OSNAME)/$(ARCHNAME)..."
#	@echo "*** ---------------------------------------------------------"

installincludes:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing C include files for $(OSNAME)/$(ARCHNAME)..."
	@echo "*** ---------------------------------------------------------"
	-mkdir -p $(REALLIBDIR)/include
	-mkdir -p $(INSTALLEDINCLUDEDIR)
	-cp $(NODEBUGSRCINCLUDEDIR)/* $(INSTALLEDINCLUDEDIR)

uninstallincludes:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Uninstalling C include files for $(OSNAME)/$(ARCHNAME)..."
	@echo "*** ---------------------------------------------------------"
	-rm -rf $(INSTALLEDINCLUDEDIR)

install: all justinstall

justinstall:
	@echo "*** ========================================================="
	@echo "*** Installing ciao"
	@echo "*** ========================================================="
	-mkdir -p $(REALLIBDIR)
	$(MAKE) justinstalleng
	$(MAKE) installincludes
	cd ciaoc;   $(MAKE) install
	cd shell;   $(MAKE) install
	cd etc;     $(MAKE) install
	cd lib;     $(MAKE) install
	cd library; $(MAKE) install
ifeq ($(INSTALL_EMACS_SUPPORT),yes)
	cd emacs;   $(MAKE) install
endif
	find $(REALLIBDIR) -type d -exec chmod $(EXECMODE) {} \;
	cd doc; $(MAKE) install DOCFORMATS="$(TARDOCFORMATS)"
	@echo "*** ========================================================="
	@echo "*** Ciao installation completed"
	@echo "*** ========================================================="

uninstall:
	@echo "*** ========================================================="
	@echo "*** Uninstalling ciao"
	@echo "*** ========================================================="
	$(MAKE) uninstalleng
	$(MAKE) uninstallincludes
	cd ciaoc; $(MAKE) uninstall
	cd shell; $(MAKE) uninstall
	cd etc;     $(MAKE) uninstall
	cd $(OBJDIR); $(MAKE) uninstall
	cd lib; $(MAKE) uninstall
	cd library; $(MAKE) uninstall
	cd emacs; $(MAKE) uninstall
	cd doc; $(MAKE) uninstall DOCFORMATS=$(TARDOCFORMATS)
	-rm -r $(REALLIBDIR)
	@echo "*** ========================================================="
	@echo "*** Ciao deinstallation completed"
	@echo "*** ========================================================="

test:
	cd tests; $(MAKE) realclean suite; $(MAKE) exec_suite


clean: engclean
	cd ciaoc;          $(MAKE) clean
	cd lib;            $(MAKE) clean
	cd shell;          $(MAKE) clean
	cd emacs;          $(MAKE) clean
	cd tests; $(MAKE) clean

realclean: engrealclean
	cd ciaoc; $(MAKE) realclean
	cd lib; $(MAKE) realclean
	cd shell; $(MAKE) realclean
	cd emacs; $(MAKE) realclean
	cd tests; $(MAKE) realclean

tar:
	(cd .. ; tar cf - $(notdir $(SRC)) | gzip -c > ciao-$(VERSION).$(PATCH).tar.gz)



### Now working on these --- MCL

totalclean: cleanbackups distclean

engrealclean engclean:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Removing $(BASEMAIN) engine for all architectures..."
	@echo "*** ---------------------------------------------------------"
	-rm -r $(SRC)/bin

cleanbackups:
	(cd $(SRC); find . -name '*~' -exec /bin/rm {} \;)
	(cd $(SRC); find $(SRC) -name '#*' -exec /bin/rm {} \;)

distclean: engclean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning $(SRC) distribution tree... (unix)"
	@echo "*** ---------------------------------------------------------"
	cd ciaoc; $(MAKE) distclean
	cd shell; $(MAKE) distclean
	cd etc; $(MAKE) distclean
	$(SRC)/etc/recursive_make_or_clean $(SRC)/doc $(MAKE) distclean
	$(SRC)/etc/recursive_make_or_clean $(SRC)/lib $(MAKE) distclean
	$(SRC)/etc/recursive_make_or_clean $(SRC)/library $(MAKE) distclean
	cd tests; $(MAKE) distclean

cflow:
	cd ${OBJDIR}; cflow -i -D${CIAOARCH} *.c > ${SRC}/etc/cflow.out

cxref:
	cd $(OBJDIR); cxref -xref-function -D$(ARCHNAME) -D$(OSNAME) $(THREAD_FLAG) $(FOREIGN_FILES_FLAG) *.[ch] -O$(SRC)/etc/cxref

