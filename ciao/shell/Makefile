# Copyright (C) 1997, UPM-CLIP

# This Makefile *needs* GNU make

# make ciaosh     compiles the ciao interactive toplevel
# make ciao-shell compiles the ciao-script runtime

include ../../makedir/ciaosetup_modules/makefile__common
include ../SETTINGS
include ../SHARED

# Interactive Prolog top-level
TOPLEVELSHELL = ciaosh
TOPLEVELSHELLFULL = $(TOPLEVELSHELL)-$(VERSION)$(CIAOSUFFIX)
# Used to execute prolog scripts
SCRIPTSHELL = ciao-shell
SCRIPTSHELLFULL = $(SCRIPTSHELL)-$(VERSION)$(CIAOSUFFIX)
# To generate asr files
GENASRFULL=gen_asr$(CIAOSUFFIX)

# The ciao compiler (ciaoc) takes cares of the dependencies itself. 

all:
	$(MAKE) toplevelshell
	$(MAKE) scriptshell
	$(MAKE) gen_asr

toplevelshell:
	@$(BOLD_MESSAGE) "Compiling '$(TOPLEVELSHELL)'"
	@$(SETLOCALCIAO) $(CIAOC) -x $(CIAOCOPTS) -o \
		$(SRCBINDIR)/$(TOPLEVELSHELLFULL) $(TOPLEVELSHELL)
	@-rm -f $(SRCBINDIR)/$(TOPLEVELSHELL).lnk $(SRCBINDIR)/$(TOPLEVELSHELL)
	@-ln -s $(TOPLEVELSHELLFULL) $(SRCBINDIR)/$(TOPLEVELSHELL)

scriptshell:
	@$(BOLD_MESSAGE) "Compiling '$(SCRIPTSHELL)'"
	@$(SETLOCALCIAO) $(CIAOC) -x -s $(CIAOCOPTS) -o \
		$(SRCBINDIR)/$(SCRIPTSHELLFULL) $(SCRIPTSHELL)

gen_asr:
	@$(BOLD_MESSAGE) "Compiling 'gen_asr'"
	@$(SETLOCALCIAO) $(CIAOC) -x -s $(CIAOCOPTS) gen_asr
	@-rm -f $(SRCBINDIR)/$(SCRIPTSHELL).lnk $(SRCBINDIR)/$(SCRIPTSHELL)
	@-ln -s $(SCRIPTSHELLFULL)   $(SRCBINDIR)/$(SCRIPTSHELL)
	@-rm -f $(GENASRFULL) gen_asr.bat

install: install_toplevelshell install_scriptshell

install_toplevelshell:
	@-mkdir -p $(BINDIR)
#	@$(BOLD_MESSAGE) "Installing '$(TOPLEVELSHELL)'"
	@-cp $(SRCBINDIR)/$(TOPLEVELSHELLFULL) $(BUILDBINDIR)/$(TOPLEVELSHELLFULL)
	@-chmod $(EXECMODE) $(BUILDBINDIR)/$(TOPLEVELSHELLFULL)
	@-rm -f $(BUILDBINDIR)/$(TOPLEVELSHELL).lnk $(BUILDBINDIR)/$(TOPLEVELSHELL)
	@-ln -s $(TOPLEVELSHELLFULL) $(BUILDBINDIR)/$(TOPLEVELSHELL)

install_scriptshell:
#	@$(BOLD_MESSAGE) "Installing '$(SCRIPTSHELL)'"
	@-cp $(SRCBINDIR)/$(SCRIPTSHELLFULL)   $(BUILDBINDIR)/$(SCRIPTSHELLFULL)
	@-chmod $(EXECMODE) $(BUILDBINDIR)/$(SCRIPTSHELLFULL)
	@-rm -f $(BUILDBINDIR)/$(SCRIPTSHELL).lnk $(BUILDBINDIR)/$(SCRIPTSHELL)
	@-ln -s $(SCRIPTSHELLFULL)   $(BUILDBINDIR)/$(SCRIPTSHELL)

uninstall: uninstall_$(INSTYPE)

uninstall_src:
	@$(BOLD_MESSAGE) "Skipping removal of shell files"

uninstall_ins:
	@/bin/rm -f $(BUILDBINDIR)/$(TOPLEVELSHELLFULL)
	@/bin/rm -f $(BUILDBINDIR)/$(SCRIPTSHELLFULL)
# Do not remove the links if another version has already been installed
	@if [ ! -f $(BUILDBINDIR)/$(TOPLEVELSHELL) ] ; then  \
	/bin/rm -f $(BUILDBINDIR)/$(TOPLEVELSHELL) ; \
	fi
	@if [ ! -f $(BUILDBINDIR)/$(SCRIPTSHELL) ] ; then  \
	/bin/rm -f $(BUILDBINDIR)/$(SCRIPTSHELL) ; \
	fi

cleanbackups:
	@/bin/rm -rf *~
cleantmpfiles:
	@/bin/rm -rf *.itf *.dep *.po *.asr
cleanexecs:
	@/bin/rm -rf $(TOPLEVELSHELLFULL) $(SCRIPTSHELLFULL) \
		    $(TOPLEVELSHELL)     $(SCRIPTSHELL)
distclean: cleanexecs cleantmpfiles

realclean: cleanexecs cleantmpfiles cleanbackups
#	Not distributed MH
#	cd examples; $(MAKE) distclean
