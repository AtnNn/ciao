include ../../SETTINGS
include ../../SHARED

include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

INSTALLED_ENGINE_PATH_TO_CHANGE=installed_engine_path
HEADER_SKEL=header.skel
HEADER=header

exe_header:
	@$(CIAOSRC)/etc/substitute_string \
		$(INSTALLED_ENGINE_PATH_TO_CHANGE) $(INSTALLEDENGINE) \
		$(HEADER_SKEL) $(HEADER)
	-@chmod $(DATAMODE) $(HEADER)

distclean:
	@/bin/rm -f *.itf *.po *.asr



