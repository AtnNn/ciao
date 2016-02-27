#------- You should not change ANYTHING in this file ----------------
#------- All customizable options are in the file ../SETTINGS -------

include ../SETTINGS
include ../SHARED

CIAOCNAME=ciaoc
ASRGEN=gen_asr

# Use the latest ciaoc (renamed as ciaoc.sta) to generate a new ciaoc.

compiler:
	cp -f $(CIAOCNAME) $(CIAOCSTA)
	-chmod $(EXECMODE) $(CIAOCSTA)
	$(SETLOCALCIAO) $(CIAOCSTA) -s -x $(CIAOCNAME)
	$(SETLOCALCIAO) $(CIAOC) -s -x $(ASRGEN)
	rm -f $(ASRGEN) $(ASRGEN).cpx

install:
	-mkdir -p $(BINDIR)
	-cp $(CIAOCNAME) $(BINDIR)/$(CIAOCNAME)-$(VERSION)
	-chmod $(EXECMODE) $(BINDIR)/$(CIAOCNAME)-$(VERSION)
	-rm -f $(BINDIR)/$(CIAOCNAME) 
	-ln -s $(CIAOCNAME)-$(VERSION) $(BINDIR)/$(CIAOCNAME) 

uninstall:
	@/bin/rm -f $(BINDIR)/$(CIAOCNAME)-$(VERSION) 
	@if [ ! -f $(BINDIR)/$(CIAOCNAME) ] ; then \
		/bin/rm -f $(BINDIR)/$(CIAOCNAME) ; \
	fi

distclean: cleantmpfiles cleanexecs

realclean: cleantmpfiles

cleanbackups:
	/bin/rm -f *~ *bak
cleantmpfiles:
	/bin/rm -f *.po *.itf *.asr

# Remember not to erase our only static compiler!
cleanexecs:
	/bin/mv $(STATICCOMPNAME) temp-$(STATICCOMPNAME)
# Mask out owner-[executable,readable,writable] files
	find . -name 'ciaoc*' -perm -000700 -exec /bin/rm {} \;
# Include also any stray .cpx compiler (sometimes not executable!)
	find . -name 'ciaoc.cpx' -exec /bin/rm {} \;
	/bin/mv temp-$(STATICCOMPNAME) $(STATICCOMPNAME)
	rm -f $(ASRGEN) $(ASRGEN).cpx
