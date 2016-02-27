include ../SETTINGS
include ../SHARED

include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

# Beware: these must be .pl files: otherwise they may be eliminated on cleanup
# get_version taken out
PLUTILITIES=fileinfo get_deps pldiff viewpo lpmake cleandirs fix_size \
            show_asr compiler_output
SHSCRIPTS=ciao_get_arch collect_modules

.SUFFIXES: .pl

.pl:
	@echo "Compiling $*.pl..." ; \
	$(SETLOCALCIAO) $(CIAOC) -x $* ; \
	chmod $(EXECMODE) $* ; 

all: comp_message $(PLUTILITIES)
# Generate shell initialization files
	@./substitute_string documentation_directory $(DOCDIR) \
		binary_directory $(BINDIR) \
		DOTcshrc.skel DOTcshrc
	@./substitute_string documentation_directory $(DOCDIR) \
		binary_directory $(BINDIR) \
		DOTprofile.skel DOTprofile
# Generate startup script
	@./substitute_string '<CIAOLIBDIR>' $(LIBDIR) \
			'CIAOVERSION=' 'CIAOVERSION=-$(VERSION)' \
			'<CIAODOCDIR>' $(DOCDIR) \
			binary_directory $(BINDIR) \
			./ciao.skel ciao-$(VERSION)
# Generate initial user message
	@./substitute_string '<LIBROOT>' $(LIBROOT) \
                             '<LPDOCDIR>' $(DOCROOT) \
	      $(SRC)/NewUser NewUser-install
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
	@-mkdir -p $(BINDIR)
	@-chmod $(EXECMODE) $(BINDIR)
# Copy shell initialization files
	@-cp DOTprofile $(REALLIBDIR)/DOTprofile
	@-chmod $(DATAMODE) $(REALLIBDIR)/DOTprofile
	@-rm -f $(LIBDIR)/DOTprofile
	@-ln -s $(REALLIBDIR)/DOTprofile $(LIBDIR)/DOTprofile
	@-cp DOTcshrc $(REALLIBDIR)/DOTcshrc
	@-chmod $(DATAMODE) $(REALLIBDIR)/DOTcshrc
	@-rm -f $(LIBDIR)/DOTcshrc
	@-ln -s $(REALLIBDIR)/DOTcshrc $(LIBDIR)/DOTcshrc
# Copy startup script
	@echo "*** ---------------------------------------------------------" 
	@echo "*** Installing ciao startup script pointing to ciao-$(VERSION)..."
	@echo "*** ---------------------------------------------------------" 
	@-rm -f $(BINDIR)/ciao-$(VERSION)
	@-cp ciao-$(VERSION) $(BINDIR)
	@-chmod $(EXECMODE) $(BINDIR)/ciao-$(VERSION)
	@-rm -f $(BINDIR)/ciao
	@-ln -s $(BINDIR)/ciao-$(VERSION) $(BINDIR)/ciao
# Copy message to new users
	@-cp NewUser-install $(REALLIBDIR)/NewUser
	@-chmod $(DATAMODE) $(REALLIBDIR)/NewUser
	@-rm -f $(LIBDIR)/NewUser
	@-ln -s $(REALLIBDIR)/NewUser $(LIBDIR)/NewUser
ifeq ($(INSTALL_PROLOG_NAME),yes)
	@echo "*** ---------------------------------------------------------" 
	@echo "*** Making 'prolog' point to ciao-$(VERSION)..."
	@echo "*** ---------------------------------------------------------" 
	@-rm -f $(BINDIR)/prolog
	@-ln -s $(BINDIR)/ciao-$(VERSION) $(BINDIR)/prolog
endif
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing utilities from etc directory..."
	@echo "*** ---------------------------------------------------------"
	@for File in $(PLUTILITIES) $(SHSCRIPTS) ; do \
             /bin/rm -f $(BINDIR)/$${File}-$(VERSION) ; \
             /bin/cp $${File} $(BINDIR)/$${File}-$(VERSION) ; \
             /bin/ln -sf $${File}-$(VERSION) $(BINDIR)/$${File} ; \
             /bin/chmod $(EXECMODE) $(BINDIR)/$${File}-$(VERSION) ; \
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
	-rm -f $(LIBDIR)/NewUser
	@echo "*** ---------------------------------------------------------"
	@echo "*** Removing utilities from etc directory..."
	@echo "*** ---------------------------------------------------------"
# Remove the files themselves and the possible dangling symbolic links 
	@-for File in $(PLUTILITIES) $(SHSCRIPTS) ; do \
	     if [ -f $(BINDIR)/$${File}-$(VERSION) ]; \
                then echo "Removing $${File}-$(VERSION)..."; \
                  rm -f $(BINDIR)/$${File}-$(VERSION) ; \
	     fi ; \
	     if [ ! -f $(BINDIR)/$${File} ]; \
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
	rm -f NewUser-install ciao-$(VERSION) DOTprofile DOTcshrc

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
# Do not remove the windows fix_size, it goes into the distribution
	@if [ -f fix_size ]; \
	  then echo "Removing fix_size"; \
               rm -f fix_size ; \
         fi

force_fix_size: 
	$(CC) -o fix_size fix_size.c

