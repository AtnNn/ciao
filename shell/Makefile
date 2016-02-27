# Copyright (C) 1997, UPM-CLIP

# This Makefile *needs* GNU make

# make ciaosh     compiles the ciao interactive toplevel
# make ciao-shell compiles the ciao-script runtime

include ../SETTINGS
include ../COMMON
include ../makefile-sysindep

# Interactive Prolog top-level
TOPLEVELSHELL = ciaosh
# Used to execute prolog scripts
SCRIPTSHELL = ciao-shell

# The ciao compiler (ciaoc) takes cares of the dependencies itself. 

all:
	$(SETLOCALCOMPILER) $(CIAOC) -x $(TOPLEVELSHELL)
	$(SETLOCALCOMPILER) $(CIAOC) -x -s $(SCRIPTSHELL)

install:
	-mkdir -p $(BINDIR)
	-cp $(TOPLEVELSHELL) $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION)
	-cp $(SCRIPTSHELL) $(BINDIR)/$(SCRIPTSHELL)-$(VERSION)
	-chmod $(EXECMODE) $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION)
	-chmod $(EXECMODE) $(BINDIR)/$(SCRIPTSHELL)-$(VERSION)
	-rm -f $(BINDIR)/$(TOPLEVELSHELL) 
	-rm -f $(BINDIR)/$(SCRIPTSHELL) 
	-ln -s $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION) $(BINDIR)/$(TOPLEVELSHELL) 
	-ln -s $(BINDIR)/$(SCRIPTSHELL)-$(VERSION) $(BINDIR)/$(SCRIPTSHELL) 


uninstall:
	/bin/rm -f $(BINDIR)/$(TOPLEVELSHELL) $(BINDIR)/$(SCRIPTSHELL) 
	/bin/rm -f $(BINDIR)/$(TOPLEVELSHELL)-$(VERSION)
	/bin/rm -f $(BINDIR)/$(SCRIPTSHELL)-$(VERSION)

cleanbackups:
	/bin/rm -rf *.~
cleantmpfiles:
	/bin/rm -rf *.itf *.dep *.po
cleanexecs: 
	/bin/rm -rf $(TOPLEVELSHELL) $(SCRIPTSHELL)
distclean: cleanexecs cleantmpfiles
#	Not distributed MH
#	cd examples; $(MAKE) distclean
