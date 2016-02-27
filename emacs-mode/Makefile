# ----------------------------------------------------------------------------
# Makefile used to build and install the Ciao-related emacs files
# You should probably not change anything in here (see ../SETTINGS)
# 
# Here is how this all works: 
# 
# - During installation (make install): 
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
# - Generating the documentation (make doc): 
#   * CiaoMode.lpdoc is generated from ciao.el.body using emacs
#     (contains the documentation for all the functions in
#     ciao.el.body). CiaoMode.pl is included as a chapter in the Ciao
#     manual.
# 
# - Generating a standalone manual for ciao.el (make dvi): 
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
ICONS = ciaopl.xpm ciao.xpm ciaoload.xpm find_error.xpm \
	   clear.xpm ciaoasr.xpm checkassertions.xpm ciaodebug.xpm \
           lpdoc.xpm lpdocview.xpm ciaoexeout.xpm ciaoitf.xpm \
	   ciaopo.xpm ciaopreprocask.xpm ciaopreproc.xpm ciaopreprocsee.xpm \
           ciaoscrt.xpm manuals.xpm wordhelp.xpm complete.xpm ciaoprompt.xpm \
           ciaorehighlight.xpm ciaostorequery.xpm stop.xpm \
           ciaoclearquery.xpm clip.xpm 
REALLIBDIR = $(LIBDIR)/$(VERSIONMAIN)
ICONDIR = $(LIBDIR)/$(VERSIONMAIN)/icons
#-----------------------------------------------------------------------------

compile:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Compiling emacs library files"
	@echo "*** ---------------------------------------------------------"
# Generate DOTemacs
	$(SUBSTITUTE_STRING) "<CIAOLIBDIR>" $(LIBDIR) \
                             "<LPDOCDIR>"   $(DOCDIR) \
	     DOTemacs.skel DOTemacs.el
	-chmod $(DATAMODE) DOTemacs.el
	-chgrp $(INSTALLGROUP) DOTemacs.el
# Generate commented DOTemacs, CiaoMode
	$(SUBSTITUTE_STRING) '^' ';;' DOTemacs.el DOTemacs.tmp
	$(SUBSTITUTE_STRING) '^' ';;' CiaoMode.pl CiaoMode.pl.tmp
	-chmod $(DATAMODE) DOTemacs.tmp
	-chgrp $(INSTALLGROUP) DOTemacs.tmp
	-chmod $(DATAMODE) CiaoMode.pl.tmp
	-chgrp $(INSTALLGROUP) CiaoMode.pl.tmp
# Generate ciao.el by collating the body and the commented versions
	$(SUBSTITUTE_STRING) \
	     'ciao-mode-version "Development Version"' \
	            'ciao-mode-version "$(VERSION)p$(PATCH)"' \
	     ciao.el.body ciao.el.body.tmp
	cat ciao.el.header DOTemacs.tmp CiaoMode.pl.tmp ciao.el.body.tmp \
	    > ciao.el.tmp
	-chmod $(DATAMODE) ciao.el.tmp
	-chgrp $(INSTALLGROUP) ciao.el.tmp
	$(SUBSTITUTE_STRING) \
	    '<CIAOREALLIBDIR>' $(REALLIBDIR) \
	    '<LPDOCDIR>' $(DOCDIR) \
	     ciao.el.tmp ciao.el
	-chmod $(DATAMODE) ciao.el
	-chgrp $(INSTALLGROUP) ciao.el
	-rm -rf word-help.elc ciao.elc
	emacs -batch -l word-help.el -l ciao.el -f compile-ciao-mode
	-chmod $(DATAMODE) ciao.elc 
	-chgrp $(INSTALLGROUP) ciao.elc 
	-chmod $(DATAMODE) word-help.elc 
	-chgrp $(INSTALLGROUP) word-help.elc 


install: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing emacs library files in $(REALLIBDIR)..."
	@echo "*** ---------------------------------------------------------"
# Make directories
	-mkdir -p $(REALLIBDIR) $(ICONDIR)
	-chmod $(EXECMODE) $(REALLIBDIR) $(ICONDIR)
	-chgrp $(INSTALLGROUP) $(REALLIBDIR) $(ICONDIR)
# Copy icons
	-cd $(ICONDIR) && 'rm' -f $(ICONS)
	cd icons && cp $(ICONS) $(ICONDIR)
	-cd $(ICONDIR) && chmod $(DATAMODE) $(ICONS) && \
            chgrp $(INSTALLGROUP) $(ICONS) && 'rm' -f *~
# Copy DOTemacs.el, set mode and owner
	-cp DOTemacs.el $(REALLIBDIR)/DOTemacs.el
	-chmod $(DATAMODE) $(REALLIBDIR)/DOTemacs.el
	-chgrp $(INSTALLGROUP) $(REALLIBDIR)/DOTemacs.el
# Copy ciao.el, set mode and owner
	-cp ciao.el $(REALLIBDIR)/ciao.el
	-chmod $(DATAMODE) $(REALLIBDIR)/ciao.el
	-chgrp $(INSTALLGROUP) $(REALLIBDIR)/ciao.el
# Copy ciao.elc, set mode and owner
	-cp ciao.elc $(REALLIBDIR)/ciao.elc
	-chmod $(DATAMODE) $(REALLIBDIR)/ciao.elc
	-chgrp $(INSTALLGROUP) $(REALLIBDIR)/ciao.elc
# Copy word-help.el, set mode and owner
	-cp word-help.el $(REALLIBDIR)/word-help.el
	-chmod $(DATAMODE) $(REALLIBDIR)/word-help.el
	-chgrp $(INSTALLGROUP) $(REALLIBDIR)/word-help.el
# Copy word-help.elc, set mode and owner
	-cp word-help.elc $(REALLIBDIR)/word-help.elc
	-chmod $(DATAMODE) $(REALLIBDIR)/word-help.elc 
	-chgrp $(INSTALLGROUP) $(REALLIBDIR)/word-help.elc 
# Set up links for all of the above
	-cd $(LIBDIR) && \
	'rm' -f ciao.el       && ln -s $(REALLIBDIR)/ciao.el             && \
	'rm' -f ciao.elc      && ln -s $(REALLIBDIR)/ciao.elc            && \
	'rm' -f word-help.el  && ln -s $(REALLIBDIR)/word-help.el        && \
	'rm' -f word-help.elc && ln -s $(REALLIBDIR)/word-help.elc       && \
	'rm' -f prolog.el     && ln -s $(REALLIBDIR)/ciao.el prolog.el   && \
	'rm' -f prolog.elc    && ln -s $(REALLIBDIR)/ciao.elc prolog.elc && \
	'rm' -f DOTemacs.el   && ln -s $(REALLIBDIR)/DOTemacs.el

.PHONY: doc dvi info

doc CiaoMode.pl CiaoMode.lpdoc: ciao.el.body 
	emacs -batch -l ./word-help.el -l ./ciao.el.body \
	      -f ciao-mode-documentation
	-chmod $(DATAMODE) CiaoMode.lpdoc
	-chgrp $(INSTALLGROUP) CiaoMode.lpdoc
	touch CiaoMode.pl

dvi: CiaoMode.lpdoc
	cd doc &&  $(MAKE) dvi

view: dvi
	cd doc &&  $(MAKE) view

info: CiaoMode.lpdoc
	cd doc &&  $(MAKE) info

infoview: info
	cd doc &&  $(MAKE) infoview

# ----------------------------------------------------------------------------

uninstall: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Uninstalling emacs library files from $(REALLIBDIR) ..."
	@echo "*** ---------------------------------------------------------"
	-( cd $(ICONDIR) && 'rm' -f $(ICONS) )
	'rm' -f $(LIBDIR)/DOTemacs.el $(LIBDIR)/ciao.el $(LIBDIR)/ciao.elc 
	'rm' -f $(LIBDIR)/word-help.el $(LIBDIR)/word-help.elc 
	'rm' -f $(LIBDIR)/prolog.el $(LIBDIR)/prolog.elc 
	'rm' -f $(REALLIBDIR)/DOTemacs.el $(REALLIBDIR)/ciao.el \
	        $(REALLIBDIR)/ciao.elc 
	'rm' -f $(REALLIBDIR)/word-help.el $(REALLIBDIR)/word-help.elc
	-rmdir $(ICONDIR)
	-rmdir $(REALLIBDIR)
#-----------------------------------------------------------------------------
clean:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up $(BASEMAIN) emacs directory..."
	@echo "*** ---------------------------------------------------------"
	-rm -f DOTemacs.tmp CiaoMode.pl.tmp ciao.el.body.tmp ciao.el.tmp
	'rm' -f *~

realclean distclean: clean
	'rm' -f CiaoMode.lpdoc
	'rm' -f *.itf *.po *.asr
	'rm' -f *.elc
# END
#-----------------------------------------------------------------------------

