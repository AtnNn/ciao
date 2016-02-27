#
# Makefile for aprolog (run with gmake)
# by pts@fazekas.hu at Fri Jan 13 17:58:18 CET 2006
#
# !! why so much memory allocation w/o builtin.apl?: e.g. `aprolog.bin bootlib/conv.apl'
# !! SWI-Prolog is noncomforming. Should be:
#    -N0*B-V == (-N0)*B-V
# !! SWI-Prolog is noncomforming. Should be:
#    0'\\ -> 92 and 0'\' -> 39 for SWI-Prolog
# !! is length/2 built-in?
# !! add: X is integer(5.6)
# Dat: no cyclic terms
# 

CC = gcc
CFLAGS = $(OFLAGS) -W -Wall -O3 \
  -Wstrict-prototypes -Wnested-externs \
  -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wmissing-prototypes \
  -Wmissing-declarations
# -Winline

LDFLAGS = -s
OFLAGS = -O3

#LDFLAGS =
#OFLAGS = -g

#CC = cc
#CFLAGS = -xO5 -Dinline=

.PHONY: all clean rebootstrap rebootstrap-swi rebootstrap-swi-conv
all: aprolog.bin bootlib/conv.apl bootlib/builtin.apl

aprolog.o: aprolog.c aprolog.h
	$(CC) $(CFLAGS) -c -o $@ $<
builtins.o: builtins.c aprolog.h
	$(CC) $(CFLAGS) -c -o $@ $<
readprog.o: readprog.c aprolog.h
	$(CC) $(CFLAGS) -c -o $@ $<
expr.o: expr.c aprolog.h
	$(CC) $(CFLAGS) -c -o $@ $<

aprolog_objs = aprolog.o builtins.o readprog.o expr.o
aprolog.bin: $(aprolog_objs)
	$(CC) $(LDFLAGS) -o $@ $(aprolog_objs) -lm

clean:
	rm -f *~ core gmon.out aprolog.prof DEADJOE aprolog.bin aprolog
	rm -f *.o *.s parser/*.o parser/libparse.a conv/conv

# ---

APROLOG_DEPS= aprolog-run aprolog.bin bootlib/builtin.apl

SOURCES = arith.pl database.pl dcg.pl debug.pl err.pl \
    execution.pl io.pl char_io.pl term_io.pl lists.pl misc.pl \
    rdtok.pl read.pl readprog.pl setof.pl term.pl write.pl

bootsrc/builtin.apl: $(addprefix bootsrc/,$(SOURCES)) $(APROLOG_DEPS) bootlib/conv.apl
	rm -f $@
	exec >$@.tmp; \
	for F in $(addprefix bootsrc/,$(SOURCES)); do \
	  echo Converting $$F >&2; \
	  ./aprolog-run bootlib/conv.apl <$$F || exit 2; \
	done
	mv $@.tmp $@

bootsrc/conv.apl: bootsrc/conv.pl $(APROLOG_DEPS) bootlib/conv.apl
	rm -f $@
	./aprolog-run bootlib/conv.apl <bootsrc/conv.pl >$@.tmp
	mv $@.tmp $@

rebootstrap:
	rm -f bootsrc/conv.apl bootsrc/builtin.apl
	$(MAKE) bootsrc/conv.apl bootsrc/builtin.apl
	cp bootsrc/conv.apl bootsrc/builtin.apl bootlib/


# Imp: faster, invoke `swiprolog' only once
bootsrc/builtin.swipl.apl: $(addprefix bootsrc/,$(SOURCES)) bootsrc/conv.swipl.sh
	rm -f $@
	exec >$@.tmp; \
	for F in $(SOURCES); do \
	  echo Converting bootsrc/$$F >&2; \
	  (cd bootsrc && ./conv.swipl.sh <$$F || exit 2); \
	done
	mv $@.tmp $@

bootsrc/conv.swipl.apl: bootsrc/conv.pl bootsrc/conv.swipl.sh
	rm -f $@
	cd bootsrc && ./conv.swipl.sh <conv.pl >conv.swipl.apl.tmp
	mv $@.tmp $@

rebootstrap-swipl:
	rm -f bootsrc/conv.swipl.apl bootsrc/builtin.swipl.apl
	$(MAKE) bootsrc/conv.swipl.apl bootsrc/builtin.swipl.apl
	cp bootsrc/conv.swipl.apl bootlib/conv.apl
	cp bootsrc/builtin.swipl.apl bootlib/builtin.apl


# Imp: faster, invoke SICStus only once
bootsrc/builtin.sicstus.apl: $(addprefix bootsrc/,$(SOURCES)) bootsrc/conv.sicstus.sh
	rm -f $@
	exec >$@.tmp; \
	for F in $(SOURCES); do \
	  echo Converting bootsrc/$$F >&2; \
	  (cd bootsrc && ./conv.sicstus.sh <$$F || exit 2); \
	done
	mv $@.tmp $@

bootsrc/conv.sicstus.apl: bootsrc/conv.pl bootsrc/conv.sicstus.sh
	rm -f $@
	cd bootsrc && ./conv.sicstus.sh <conv.pl >conv.sicstus.apl.tmp
	mv $@.tmp $@

rebootstrap-sicstus:
	rm -f bootsrc/conv.sicstus.apl bootsrc/builtin.sicstus.apl
	$(MAKE) bootsrc/conv.sicstus.apl bootsrc/builtin.sicstus.apl
	cp bootsrc/conv.sicstus.apl bootlib/conv.apl
	cp bootsrc/builtin.sicstus.apl bootlib/builtin.apl
