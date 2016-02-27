# Copyright (C) 1997, UPM-CLIP

# This Makefile *needs* GNU make

# make ciaosh     compiles the ciao interactive toplevel
# make ciao-shell compiles the ciao-script runtime

include ../SETTINGS
include ../SHARED

# Interactive Prolog top-level
TOPLEVELSHELL = ciaosh
# Used to execute prolog scripts
SCRIPTSHELL = ciao-shell
# To generate asr files
ASRGEN=gen_asr

# The ciao compiler (ciaoc) takes cares of the dependencies itself. 

all:
	$(SETLOCALCIAO) $(CIAOC) -x $(TOPLEVELSHELL)
	$(SETLOCALCIAO) $(CIAOC) -x -s $(SCRIPTSHELL)
	$(SETLOCALCIAO) $(CIAOC) -s -x $(ASRGEN)
	rm $(ASRGEN)

install:
	-mkdir -p $(BINDIR)
	-cp $(TOPLEVELSHELL) $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION)
	-cp $(SCRIPTSHELL) $(BINDIR)/$(SCRIPTSHELL)-$(VERSION)
	-chmod $(EXECMODE) $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION)
	-chmod $(EXECMODE) $(BINDIR)/$(SCRIPTSHELL)-$(VERSION)
	-rm -f $(BINDIR)/$(TOPLEVELSHELL) 
	-rm -f $(BINDIR)/$(SCRIPTSHELL) 
	-ln -s $(TOPLEVELSHELL)-$(VERSION) $(BINDIR)/$(TOPLEVELSHELL) 
	-ln -s $(SCRIPTSHELL)-$(VERSION) $(BINDIR)/$(SCRIPTSHELL) 


uninstall:
	@/bin/rm -f $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION)
	@/bin/rm -f $(BINDIR)/$(SCRIPTSHELL)-$(VERSION)
# Do not remove the links if another version has already been installed
	@if [ ! -f $(BINDIR)/$(TOPLEVELSHELL) ] ; then  \
	/bin/rm -f $(BINDIR)/$(TOPLEVELSHELL) ; \
	fi
	@if [ ! -f $(BINDIR)/$(SCRIPTSHELL) ] ; then  \
	/bin/rm -f $(BINDIR)/$(SCRIPTSHELL) ; \
	fi


cleanbackups:
	/bin/rm -rf *.~
cleantmpfiles:
	/bin/rm -rf *.itf *.dep *.po *.asr
cleanexecs: 
	/bin/rm -rf $(TOPLEVELSHELL) $(SCRIPTSHELL)
distclean: cleanexecs cleantmpfiles
#	Not distributed MH
#	cd examples; $(MAKE) distclean
