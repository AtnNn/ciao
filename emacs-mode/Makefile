# ----------------------------------------------------------------------------
# Makefile used to build and install the Ciao-related emacs files
# You should probably not change anything in here (see ../ciao/SETTINGS)
# 
# Here is how this all works: 
# 
# - During installation (make install): 
#   * Paths in ciao-mode-init.el.skel are fixed and ciao-mode-init.el (which contains
#     the suggestions at to what users should put in their .emacs files)
#     generated and installed in the libraries.
#   * ciao.el.header, a commented version of ciao-mode-init.el, 
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

# CIAODESRC=$(PWD)/..
SRC=$(CIAODESRC)

-include ../ciao/SETTINGS
-include ../ciao/SHARED
SUBSTITUTE_STRING=../ciao/etc/substitute_string
#include /home/clip/Systems/ciao/SETTINGS
#include /home/clip/Systems/ciao/SHARED
#SUBSTITUTE_STRING = /home/clip/Systems/ciao/etc/substitute_string
#-----------------------------------------------------------------------------
# Second set includes grey background (for xemacs)
ICONS = cancel2.png cb_arrow.png go.png  \
        checkassertions.xpm ciao.xpm  \
	ciaoanalysis.xpm ciaoasr.xpm \
        ciaoclearquery.xpm ciaocustomize.xpm \
	ciaodebug.xpm ciaoexeout.xpm \
	ciaoitf.xpm ciaoload.xpm ciaopeval.xpm \
	ciaopl.xpm ciaopo.xpm \
	ciaopreproc.xpm ciaopreprocask.xpm ciaopreprocsee.xpm \
	ciaoprompt.xpm ciaorehighlight.xpm \
	ciaoscrt.xpm ciaostorequery.xpm \
	clear.xpm clip.xpm close.xpm \
	complete.xpm copy.xpm cut.xpm \
	find_error.xpm jump_to.xpm lpdoc.xpm left-arrow.xpm \
	right-arrow.xpm lpdocview.xpm  manuals.xpm \
	open.xpm paste.xpm preferences.xpm print.xpm \
        save.xpm saveas.xpm \
	search.xpm stop.xpm undo.xpm wordhelp.xpm \
	\
	cancel2-bg.png cb_arrow-bg.png go-bg.png \
        checkassertions-bg.xpm ciao-bg.xpm \
	ciaoanalysis-bg.xpm ciaoasr-bg.xpm \
        ciaoclearquery-bg.xpm ciaocustomize-bg.xpm \
	ciaodebug-bg.xpm ciaoexeout-bg.xpm \
	ciaoitf-bg.xpm ciaoload-bg.xpm ciaopeval-bg.xpm \
	ciaopl-bg.xpm ciaopo-bg.xpm \
	ciaopreproc-bg.xpm ciaopreprocask-bg.xpm ciaopreprocsee-bg.xpm \
	ciaoprompt-bg.xpm ciaorehighlight-bg.xpm \
	ciaoscrt-bg.xpm ciaostorequery-bg.xpm \
	clear-bg.xpm clip-bg.xpm close-bg.xpm \
	complete-bg.xpm copy-bg.xpm cut-bg.xpm \
	find_error-bg.xpm jump_to-bg.xpm lpdoc-bg.xpm left-arrow-bg.xpm \
	right-arrow-bg.xpm lpdocview-bg.xpm  manuals-bg.xpm \
	open-bg.xpm paste-bg.xpm preferences-bg.xpm print-bg.xpm \
	save-bg.xpm saveas-bg.xpm \
	search-bg.xpm stop-bg.xpm undo-bg.xpm wordhelp-bg.xpm
PICONS = cancel2.png    cb_arrow.png    go.png \
	 cancel2-bg.png cb_arrow-bg.png go-bg.png
ICONDIR = $(LIBDIR)/$(VERSIONMAIN)/icons
BUILDICONDIR = $(BUILD_ROOT)$(ICONDIR)
#-----------------------------------------------------------------------------

ifeq ($(INSTYPE),src)
CIAOLIBDIR=$(CIAODESRC)/emacs-mode
CIAOREALLIBDIR=$(CIAOSRC)
LPDOCLIBDIR=$(CIAODESRC)/lpdoc/lib
else
CIAOLIBDIR=$(LIBDIR)
CIAOREALLIBDIR=$(REALLIBDIR)
LPDOCLIBDIR=$(LIBROOT)/lpdoc
endif

compile:
	cd ../ciao ; $(LPMAKECMD) emacs_mode_compile

install: install_msg install_common install_xemacs_$(INSTALL_XEMACS_SUPPORT) \
	install_emacs_$(INSTALL_EMACS_SUPPORT) install_links

install_msg:
	@echo ":: Installing emacs library files in $(REALLIBDIR)..."

install_common:
# Make directories
	-mkdir -p $(BUILDREALLIBDIR) $(BUILDICONDIR) $(BUILDCIAOMODEINITDIR) \
		$(BUILDXEMACSINITDIR)
	-chmod $(EXECMODE) $(BUILDREALLIBDIR) $(BUILDICONDIR)
# Copy icons
	-cd $(BUILDICONDIR) && rm -f $(ICONS)
	cd icons && cp $(ICONS) $(BUILDICONDIR)
	-cd $(BUILDICONDIR) && chmod $(DATAMODE) $(ICONS) && rm -f *~
# Copy png icons
	-cd $(BUILDREALLIBDIR) && rm -f $(PICONS)
	cd icons && cp $(PICONS) $(BUILDREALLIBDIR) 
	-cd $(BUILDREALLIBDIR) && chmod $(DATAMODE) $(PICONS) && rm -f *~
# Copy ciao.el, set mode and owner
	-cp ciao.el $(BUILDREALLIBDIR)/ciao.el
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/ciao.el
# Copy ciao.elc, set mode and owner
	-cp ciao.elc $(BUILDREALLIBDIR)/ciao.elc
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/ciao.elc
# Copy word-help.el, set mode and owner
	-cp word-help.el $(BUILDREALLIBDIR)/word-help.el
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/word-help.el
# Copy word-help.elc, set mode and owner
	-cp word-help.elc $(BUILDREALLIBDIR)/word-help.elc
	-chmod $(DATAMODE) $(BUILDREALLIBDIR)/word-help.elc 
ifneq ($(INSTALLGROUP),)
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR) $(BUILDICONDIR)
	-cd $(BUILDICONDIR) && chgrp $(INSTALLGROUP) $(ICONS)
	-cd $(BUILDREALLIBDIR) && chgrp $(INSTALLGROUP) $(PICONS)
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/ciao.el
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/ciao.elc
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/word-help.el
	-chgrp $(INSTALLGROUP) $(BUILDREALLIBDIR)/word-help.elc 
endif

install_links:
# Set up links for all of the above
	-cd $(BUILDLIBDIR) && \
	  rm -f ciao.el       && ln -s $(RELREALLIBDIR)/ciao.el             && \
	  rm -f ciao.elc      && ln -s $(RELREALLIBDIR)/ciao.elc            && \
	  rm -f word-help.el  && ln -s $(RELREALLIBDIR)/word-help.el        && \
	  rm -f word-help.elc && ln -s $(RELREALLIBDIR)/word-help.elc       && \
	  rm -f prolog.el     && ln -s $(RELREALLIBDIR)/ciao.el prolog.el   && \
	  rm -f prolog.elc    && ln -s $(RELREALLIBDIR)/ciao.elc prolog.elc

install_emacs_ install_emacs_yes:
# Copy ciao-mode-init.el, set mode and owner
	-cp ciao-mode-init.el $(BUILDCIAOMODEINITDIR)/$(EMACSINITFILE)
	-chmod $(DATAMODE) $(BUILDCIAOMODEINITDIR)/$(EMACSINITFILE)
ifneq ($(INSTALLGROUP),)
	-chgrp $(INSTALLGROUP) $(BUILDCIAOMODEINITDIR)/$(EMACSINITFILE)
endif
	-cd $(BUILDLIBDIR) && \
	  rm -f ciao-mode-init.el && \
	  ln -s $(CIAOMODEINITDIR)/$(EMACSINITFILE) ciao-mode-init.el

install_emacs_no:

install_xemacs_ install_xemacs_yes:
# Copy ciao-mode-init.el, set mode and owner
	-cp ciao-mode-init.el $(BUILDXEMACSINITDIR)/$(XEMACSINITFILE)
	-chmod $(DATAMODE) $(BUILDXEMACSINITDIR)/$(XEMACSINITFILE)
ifneq ($(INSTALLGROUP),)
	-chgrp $(INSTALLGROUP) $(BUILDXEMACSINITDIR)/$(XEMACSINITFILE)
endif
	-cd $(BUILDLIBDIR) && \
	  rm -f ciao-mode-init.el && \
	  ln -s $(XEMACSINITDIR)/$(XEMACSINITFILE) ciao-mode-init.el

install_xemacs_no:

.PHONY: doc dvi info

docs CiaoMode.pl CiaoMode.lpdoc: ciao.el
	$(EMACS_FOR_CIAO) -batch -l ./word-help.el -l ./ciao.el \
	      -f ciao-mode-documentation
	-chmod $(DATAMODE) CiaoMode.lpdoc
ifneq ($(INSTALLGROUP),)
	-chgrp $(INSTALLGROUP) CiaoMode.lpdoc
endif
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

uninstall: uninstall_$(INSTYPE)
uninstall_src:
	@echo: "NOTE: Skipping removal of emacs files ..."

uninstall_ins: uninstall_xemacs_$(INSTALL_XEMACS_SUPPORT) \
	uninstall_emacs_$(INSTALL_EMACS_SUPPORT) uninstall_common

uninstall_common:
	-( cd $(BUILDICONDIR) && rm -f $(ICONS) )
	-( cd $(BUILDREALLIBDIR) && rm -f $(PICONS) )
	-rm -f $(BUILDLIBDIR)/ciao-mode-init.el \
		$(BUILDLIBDIR)/ciao.el \
		$(BUILDLIBDIR)/ciao.elc 
	-rm -f $(BUILDLIBDIR)/word-help.el $(BUILDLIBDIR)/word-help.elc 
	-rm -f $(BUILDLIBDIR)/prolog.el $(BUILDLIBDIR)/prolog.elc 
	-rm -f $(BUILDREALLIBDIR)/ciao-mode-init.el \
		$(BUILDREALLIBDIR)/ciao.el \
		$(BUILDREALLIBDIR)/ciao.elc 
	-rm -f $(BUILDREALLIBDIR)/word-help.el \
		$(BUILDREALLIBDIR)/word-help.elc
	-rmdir $(BUILDICONDIR)

uninstall_emacs_no:

uninstall_emacs_ uninstall_emacs_yes:
	@echo "{Uninstalling emacs library files from $(REALLIBDIR)}"
	-rm -f $(BUILDCIAOMODEINITDIR)/$(EMACSINITFILE)

uninstall_xemacs_no:

uninstall_xemacs_ uninstall_xemacs_yes:
	@echo "{Uninstalling xemacs library files from $(REALLIBDIR)}"
	-rm -f $(BUILDXEMACSINITDIR)/$(XEMACSINITFILE)

#-----------------------------------------------------------------------------
clean:
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up $(BASEMAIN) emacs directory..."
	@echo "*** ---------------------------------------------------------"
	-rm -f ciao-mode-init.tmp CiaoMode.pl.tmp ciao.el.tmp \
		ciao-mode-init.el *~

realclean distclean: clean
	rm -f CiaoMode.lpdoc *.itf *.po *.asr -f *.elc ciao.el
# END
#-----------------------------------------------------------------------------

