DOMAIN=q

links: clean
	cd ../clpqr_src ; \
	for File in *.pl ; do \
	    F=$${File%.pl} ; \
	    ln -s ../clpqr_src/$$File ../clp$(DOMAIN)/$${F%_$(DOMAIN)}.pl ; \
	done

clean:
	find . -type l -exec rm -f '{}' \;

distclean:

