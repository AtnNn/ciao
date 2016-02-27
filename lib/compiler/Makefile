include ../../SETTINGS
include ../../SHARED

include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

TO_CHANGE=substitute_this_text_by_the_installed_engine_path
HEADER_SKEL=header_skeleton
HEADER=header

exe_header:
	$(SRC)/etc/substitute_string $(TO_CHANGE) $(INSTALLEDENGINE) $(HEADER_SKEL) $(HEADER)
	chmod $(DATAMODE) $(HEADER)

distclean:
	/bin/rm -f *.itf *.po *.asr
	/bin/rm -f browse_builtins
	/bin/rm -f browse_builtins.cpx


