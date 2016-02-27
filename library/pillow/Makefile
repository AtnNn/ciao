#        PiLLoW 1.1                             #
#                                               #
#  Copyright (C) 1996-1997-1998-1999-2000 CLIP  #
#             see file COPYING.LIB              #
#################################################
include ../../SETTINGS
include ../../SHARED

pillow: icon_address.pl
	$(SETLOCALCIAO) $(CIAOC) -x aux.pl ; rm -f aux aux.itf aux.po

icon_address.pl: ../../SETTINGS
	echo "icon_base_address('$(WEB_IMAGES_URL)')." > icon_address.pl

install:
	@if test ! -d $(WEB_IMAGES_PATH) ; then \
          echo 'Warning: The value of WEB_IMAGES_PATH=$(WEB_IMAGES_PATH) is not properly set, Pillow will not be correctly configured'; \
	else \
          /bin/cp -pfr images/* $(WEB_IMAGES_PATH); \
	fi

dist: Makefile \
      html.pl http.pl ops.pl pillow_aux.pl pillow_doc.pl pillow_types.pl
	cd dist ; $(MAKE)

distclean:
	/bin/rm -f *.asr *.itf *.po
