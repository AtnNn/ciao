
build: simple_server.pl simple_client_with_main.pl
	ciaoc -a 'actmods/filebased_publish' simple_server
	ciaoc simple_client_with_main

distclean:
	/bin/rm -f *.po *.itf *.dep *~ *.addr \
	tmpciao* simple_server simple_client_with_main
