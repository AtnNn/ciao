# ----------------------------------------------------------------------------
# Makefile used to build and install the Ciao-related emacs files
# You should probably not change anything in here (see ../SETTINGS)
# 
# Here is how this all works: 
# 
# - During installation (gmake install): 
#   * Paths in DOTemacs.skel are fixed and DOTemacs.el (which contains
#     the suggestions at to what users should put in their .emacs files)
#     generated and installed in the libraries.
#   * ciao.el.header, a commented version of DOTemacs.el, 
#     ciao.el.body, and a commented version of CiaoMode.pl (which contains
#     version info)  are concatenated to produce ciao.el, which is
#     installed in the libraries. A link to it called prolog.el is made.
#     The current Ciao version is also included in ciao.el at this time.
#   * word-help.el is also installed in the libraries.
#   * All .el files in the libraries are byte-compiled.
#    
# - Version control: is now done automatically on CiaoMode.pl upon saving 
#   ciao.el.body.
# 
# - Generating the documentation (gmake doc): 
#   * CiaoMode.lpdoc is generated from ciao.el.body using emacs
#     (contains the documentation for all the functions in
#     ciao.el.body). CiaoMode.pl is included as a chapter in the Ciao
#     manual.
# 
# - Generating a standalone manual for ciao.el (gmake dvi): 
#   * Also, CiaoMode.dvi is generated in the doc directory using lpdoc
#     from CiaoMode.pl and CiaoMode.lpdoc. This is for tests only. 
#
#-----------------------------------------------------------------------------
include ../SETTINGS
include ../SHARED
SUBSTITUTE_STRING=../etc/substitute_string
#include /home/clip/Systems/ciao/SETTINGS
#include /home/clip/Systems/ciao/SHARED
#SUBSTITUTE_STRING = /home/clip/Systems/ciao/etc/substitute_string
#-----------------------------------------------------------------------------
LIBFILES = word-help.el ciao.xpm ciaoload.xpm ciaoasr.xpm ciaoexeout.xpm \
           ciaoitf.xpm ciaopl.xpm ciaopo.xpm ciaoscrt.xpm lpdocview.xpm \
	   clear.xpm complete.xpm wordhelp.xpm ciaomanuals.xpm manuals.xpm \
           ciaodebug.xpm stop.xpm ciaoprompt.xpm ciaostorequery.xpm \
	   ciaoclearquery.xpm
REALLIBDIR = $(LIBDIR)/$(VERSIONMAIN)
#-----------------------------------------------------------------------------
install: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing emacs library files in $(REALLIBDIR)..."
	@echo "*** ---------------------------------------------------------"
	-mkdir -p $(REALLIBDIR)
	-chmod $(EXECMODE) $(REALLIBDIR)
	cp $(LIBFILES) $(REALLIBDIR)
	-cd $(REALLIBDIR); chmod $(DATAMODE) $(LIBFILES); 'rm' -f *~
	$(SUBSTITUTE_STRING) "<CIAOLIBDIR>" $(LIBDIR) \
                                 "<LPDOCDIR>" $(DOCDIR) \
	     DOTemacs.skel $(REALLIBDIR)/DOTemacs.el
	-chmod $(DATAMODE) $(REALLIBDIR)/DOTemacs.el
	$(SUBSTITUTE_STRING) '^' ';;' \
	     $(REALLIBDIR)/DOTemacs.el \
	     $(REALLIBDIR)/DOTemacs.tmp
	$(SUBSTITUTE_STRING) '^' ';;' \
	     CiaoMode.pl \
	     $(REALLIBDIR)/CiaoMode.pl.tmp
	$(SUBSTITUTE_STRING) 'ciao-mode-version "Development Version"' \
	     'ciao-mode-version "$(VERSION)p$(PATCH)"' \
	     ciao.el.body $(REALLIBDIR)/ciao.el.body.tmp
	cat ciao.el.header $(REALLIBDIR)/DOTemacs.tmp \
	    $(REALLIBDIR)/CiaoMode.pl.tmp \
	    $(REALLIBDIR)/ciao.el.body.tmp > $(REALLIBDIR)/ciao.el.tmp
	-rm -f $(REALLIBDIR)/DOTemacs.tmp
	-rm -f $(REALLIBDIR)/CiaoMode.pl.tmp
	-rm -f $(REALLIBDIR)/ciao.el.body.tmp
	$(SUBSTITUTE_STRING) '<CIAOREALLIBDIR>' \
             $(REALLIBDIR) \
	     $(REALLIBDIR)/ciao.el.tmp \
	     $(REALLIBDIR)/ciao.el
	-rm -f $(REALLIBDIR)/ciao.el.tmp
	-chmod $(DATAMODE) $(REALLIBDIR)/ciao.el
	-cd $(REALLIBDIR); emacs -batch -l ciao.el -f compile-ciao-mode
	-chmod $(DATAMODE) $(REALLIBDIR)/ciao.elc
	-chmod $(DATAMODE) $(REALLIBDIR)/word-help.elc 
	-cd $(LIBDIR); 'rm' -f ciao.el; \
	    ln -s $(REALLIBDIR)/ciao.el
	-cd $(LIBDIR); 'rm' -f ciao.elc; \
	    ln -s $(REALLIBDIR)/ciao.elc
	-cd $(LIBDIR); 'rm' -f prolog.el; \
            ln -s $(REALLIBDIR)/ciao.el prolog.el
	-cd $(LIBDIR); 'rm' -f prolog.elc; \
            ln -s $(REALLIBDIR)/ciao.elc prolog.elc
	-cd $(LIBDIR); 'rm' -f DOTemacs.el; \
            ln -s $(REALLIBDIR)/DOTemacs.el


.PHONY: doc dvi info

doc CiaoMode.pl CiaoMode.lpdoc: ciao.el.body 
	emacs -batch -l ./word-help.el -l ./ciao.el.body \
	      -f ciao-mode-documentation
	-chmod $(DATAMODE) CiaoMode.lpdoc
	touch CiaoMode.pl

dvi: CiaoMode.lpdoc
	cd doc; $(MAKE) dvi

view: dvi
	cd doc; $(MAKE) view

info: CiaoMode.lpdoc
	cd doc; $(MAKE) info

infoview: info
	cd doc; $(MAKE) infoview

# ----------------------------------------------------------------------------

uninstall: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Uninstalling emacs library files from $(REALLIBDIR) ..."
	@echo "*** ---------------------------------------------------------"
	-( cd $(REALLIBDIR); 'rm' -f $(LIBFILES) )
	'rm' -f $(LIBDIR)/DOTemacs.el $(LIBDIR)/ciao.el $(LIBDIR)/ciao.elc 
	'rm' -f $(LIBDIR)/prolog.el $(LIBDIR)/prolog.elc 
	-rmdir $(REALLIBDIR)
#-----------------------------------------------------------------------------
clean:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up $(BASEMAIN) emacs directory..."
	@echo "*** ---------------------------------------------------------"
	'rm' -f *~

realclean distclean: clean
	'rm' -f CiaoMode.lpdoc
# END
#-----------------------------------------------------------------------------

