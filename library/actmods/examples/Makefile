
build: simple_server.pl simple_client_with_main.pl
	ciaoc -a actmods/filebased_publish simple_server
	ciaoc simple_client_with_main

webbased:
	ciaoc -a actmods/webserver_publish webbased_server
	webbased_server &
	ciaoc -a actmods/webbased_publish simple_server

distclean:
	/bin/rm -f *.po *.itf *.dep *~ *.addr \
	tmpciao* simple_server simple_client_with_main webbased_server
