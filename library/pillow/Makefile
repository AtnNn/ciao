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

pillow: icon_address
	ciaoc -c html
	ciaoc -c http

icon_address:
	echo "icon_base_address('$(ICONDIR)')." > icon_address.pl

distclean:
	/bin/rm -f *.itf *.dep *.po
