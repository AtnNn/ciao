#        PiLLoW 1.1                             #
#                                               #
#    Copyright (C) 1996-1997-1998 UPM-CLIP.     #
#             see file COPYING.LIB              #
#################################################

# ------- Options -------------------------------

## URL where images will be
#ICONDIR=http://localhost/images/
ICONDIR=http://www.clip.dia.fi.upm.es/images/

# You should not change anything below this line
# ------- End of Options ------------------------

pillow: icon_address.pl
	ciaoc -c html
	ciaoc -c http

icon_address.pl: Makefile
	echo "icon_base_address('$(ICONDIR)')." > icon_address.pl

dist: icon_address.pl common.pl html.pl http.pl
	cd dist ; $(MAKE)
