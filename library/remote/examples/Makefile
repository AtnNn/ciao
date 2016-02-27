include ../../../SETTINGS
include ../../../SHARED


all:
	$(SETLOCALCIAO) $(CIAOC) -x server
	$(SETLOCALCIAO) $(CIAOC) -x client

run:
	./server &
	sleep 2
	./client localhost


clean:
	/bin/rm -f *.po *.itf client server conc example_server

realclean distclean: clean
	/bin/rm -rf *~ tmpciao*
