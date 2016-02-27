#        PiLLoW 1.1                             #
#                                               #
#  Copyright (C) 1996-1997-1998-1999-2000 CLIP  #
#             see file COPYING.LIB              #
#################################################

# ------- Options -------------------------------

### URL where images will be
#ICONDIR=http://localhost/images/

# You should not change anything below this line
# ------- End of Options ------------------------

pillow: icon_address.pl
	ciaoc -x aux.pl ; rm -f aux aux.itf aux.po

#icon_address.pl: Makefile
#	echo "icon_base_address('$(ICONDIR)')." > icon_address.pl

dist: Makefile \
      html.pl http.pl ops.pl pillow_aux.pl pillow_doc.pl pillow_types.pl
	cd dist ; $(MAKE)
	cp -f dist/INSTALL INSTALL
	cp -f dist/README README

distclean:
	/bin/rm *.asr *.itf *.po
