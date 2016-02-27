#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------

include ../SETTINGS
include ../COMMON
include ../makefile-sysindep

CIAOCNAME=ciaoc

compiler:
	$(SETLOCALCOMPILER) $(CIAOC) -s $(CIAOCNAME)

install:
	-mkdir -p $(BINDIR)
	-cp $(CIAOCNAME) $(BINDIR)/$(CIAOCNAME)-$(VERSION)
	-chmod $(EXECMODE) $(BINDIR)/$(CIAOCNAME)-$(VERSION)
	-rm $(BINDIR)/$(CIAOCNAME) 
	-ln -s $(BINDIR)/$(CIAOCNAME)-$(VERSION) $(BINDIR)/$(CIAOCNAME) 

uninstall:
	/bin/rm -f $(BINDIR)/$(CIAOCNAME) $(BINDIR)/$(CIAOCNAME)-$(VERSION) 

distclean: cleantmpfiles cleanexecs

cleanbackups:
	/bin/rm -f *~ *bak
cleantmpfiles:
	/bin/rm -f *.po *.itf *.asr

# Remember not to erase our only static compiler!
cleanexecs:
	/bin/mv $(STATICCOMPNAME) temp-$(STATICCOMPNAME)
# Mask out owner-[executable,readable,writable] files
	find . -name 'ciaoc*' -perm -000700 -exec /bin/rm {} \;
	/bin/mv temp-$(STATICCOMPNAME) $(STATICCOMPNAME)
