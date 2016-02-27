
include ../SETTINGS
include ../COMMON
include ../makefile-sysindep

# Beware: these must be .pl files: otherwise they may be eliminated on cleanup
# get_version taken out
PLUTILITIES=fileinfo get_deps pldiff viewpo lpmake
SHSCRIPTS=ciao_get_arch collect_modules

.SUFFIXES: .pl

.pl:
	@echo "Compiling $*.pl..." ; \
	$(SETLOCALCOMPILER) $(CIAOC) -x $* ; \
	chmod $(EXECMODE) $* ; 

all: comp_message $(PLUTILITIES)
	@echo "*** ---------------------------------------------------------"
	@echo "*** Finished compiling utilities in etc directory."
	@echo "*** ---------------------------------------------------------"

comp_message:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Compiling utilities in etc directory..."
	@echo "*** ---------------------------------------------------------"

install:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing user shell initialization skeletons..."
	@echo "*** ---------------------------------------------------------"
	@if [ ! -d $(BINDIR) ]; \
             then echo "Creating $(BINDIR)..." ; \
	          mkdir -p $(BINDIR); \
	          chmod $(EXECMODE) $(BINDIR); \
	fi
	@-for File in DOTcshrc DOTprofile ; \
	 do echo "Installing $${File} (load ciao-$(VERSION) libs/docs)..." ; \
            ./substitute_string documentation_directory $(DOCDIR) \
                                binary_directory $(BINDIR) \
	                        ./$${File}.skel $(REALLIBDIR)/$${File} ; \
            chmod $(DATAMODE) $(REALLIBDIR)/$${File} ; \
	    rm -f $(LIBDIR)/$${File} ; \
	    ln -s $(REALLIBDIR)/$${File} $(LIBDIR)/$${File} ; \
	 done
	@echo "Installing ciao startup script pointing to ciao-$(VERSION)..."
	@./substitute_string '<CIAOLIBDIR>' $(LIBDIR) \
	                     'CIAOVERSION=' 'CIAOVERSION=-$(VERSION)' \
	                     '<CIAODOCDIR>' $(DOCDIR) \
	     ./ciao.skel $(BINDIR)/ciao-$(VERSION)
	@-chmod $(EXECMODE) $(BINDIR)/ciao-$(VERSION)
	@-rm -f $(BINDIR)/ciao
	@-ln -s $(BINDIR)/ciao-$(VERSION) $(BINDIR)/ciao
	@./substitute_string '<LIBROOT>' $(LIBROOT) \
                             '<LPDOCDIR>' $(DOCROOT) \
	      $(SRC)/NewUser $(REALLIBDIR)/NewUser
	@-chmod $(DATAMODE) $(REALLIBDIR)/NewUser
	@-rm -f $(LIBDIR)/NewUser
	@-ln -s $(REALLIBDIR)/NewUser $(LIBDIR)/NewUser
ifeq ($(INSTALL_PROLOG_NAME),yes)
	@echo "Making 'prolog' point to ciao-$(VERSION)..."
	@-rm -f $(BINDIR)/prolog
	@-ln -s $(BINDIR)/ciao-$(VERSION) $(BINDIR)/prolog
endif
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing utilities from etc directory..."
	@echo "*** ---------------------------------------------------------"
	@for File in $(PLUTILITIES) $(SHSCRIPTS) ; do \
             rm -f $(BINDIR)/$${File} ; \
             cp $${File} $(BINDIR)/$${File} ; \
             chmod $(EXECMODE) $(BINDIR)/$${File} ; \
	done
	@echo "*** ---------------------------------------------------------"
	@echo "*** Finished installing scripts and utilities from etc dir."
	@echo "*** ---------------------------------------------------------"


uninstall:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Removing user initialization script skeletons..."
	@echo "*** ---------------------------------------------------------"
	-rm -f $(REALLIBDIR)/DOTprofile $(LIBDIR)/DOTprofile
	-rm -f $(REALLIBDIR)/DOTcshrc $(LIBDIR)/DOTcshrc
	-rm -f $(BINDIR)/ciao $(BINDIR)/ciao-$(VERSION)
ifeq ($(INSTALL_PROLOG_NAME),yes)
	-rm -f $(BINDIR)/prolog
endif
	-rm -f $(REALLIBDIR)/NewUser
	@echo "*** ---------------------------------------------------------"
	@echo "*** Removing utilities from etc directory..."
	@echo "*** ---------------------------------------------------------"
	@for File in $(PLUTILITIES) $(SHSCRIPTS) ; \
	 do if [ -f $(BINDIR)/$${File} ]; \
             then echo "Removing $${File}..."; \
                  rm -f $(BINDIR)/$${File} ; \
	   fi ; \
	 done
	@echo "*** ---------------------------------------------------------"
	@echo "*** Finished uninstalling scripts and utilities from etc dir."
	@echo "*** ---------------------------------------------------------"

clean:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up etc directory..."
	@echo "*** ---------------------------------------------------------"
	-'rm' -f *~
	-'rm' -f *.po
	-'rm' -f *.itf
	-'rm' -f *.asr

distclean realclean: clean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Removing executables in etc directory..."
	@echo "*** ---------------------------------------------------------"
	@for File in $(PLUTILITIES) ; \
	 do if [ -f $${File}  ]; \
             then echo "Removing $${File}"; \
                  rm -f $${File} ; \
	   fi ; \
	 done
