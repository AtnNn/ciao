#
# Makefile for Prolog ISO conformance testing
# by pts@fazekas.hu at Sat Jan 14 12:16:20 CET 2006
#
# !! | ?- X is round(float_integer_part(-1.23)).
# !! evaluate *.test_results, print summary
# !! new test for existence of user_error
# !! new test for: current_prolog_flag(version, ...) % SWI-Prolog sets only prolog_flag(version, ...) with autoload=true
# !! doc: don't allow `:- ' declarations: aprolog {Warning: testbed.pl: (;)/2 is not a valid directive}
# !! new test: module system
# !! new test: etshome mycompat.pl
# !! doc: how to add a new prolog implementation
# !! doc: let Ctrl-<C> abort the test
# !! new test: put_code(66.0) should trigger a type_error
# !! new test: 7e5 is an integer in SWI-Prolog, but 7.0e5 is an integer in SICStus Prolog
# !! new test: sign of X is Y div Z
# !! new test: arithmetic overflows
# !! new test: X is round(3.4) -> X==3 (SICStus in iso mode), X==3.0 (sicstus)
# !! new test: the standard says that the product of two big integers should trigger an int_overflow (not true in SWI-Prolog!)
# !! new test: XSB doesn't interpret \' or \n in an atom name (but '')
# !! http://goanna.cs.rmit.edu.au/~winikoff/wp/ W-Prolog in Java
# !! http://www.iro.umontreal.ca/~vaucher/XProlog/ X-Prolog in Java
# !! doc: AI::Prolog in Perl
# !! new test: functor(A,B,C) -- is no in ciao
# !! irrelevant vs dangerous; make consistent; indicate all of them in
#    .test_results, `skipped' should be a fatal errror
# !! Executable Specification for Standard Prolog by Pierre Deransat and AbdelAli Ed-Dbali
#

.PHONY: all

all: sicstus.test_results swipl.test_results aprolog.test_results ciao.test_results

sicstus.test_results:	
	rm -f $@
	rm -f *.po
	echo "main. " | sicstus-3.12.3 -l main_sicstus > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

sicstus4.test_results:
	rm -f $@
	rm -f *.po
	echo "main. " | sicstus-4.0.0beta1 -l main_sicstus4 > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

#     !! abort on Ctrl-<C>; now: segfault -- EOF on pipe
#     !! abort on a compile error
#     !! abort on failure, doc: halt(...) codes
gprolog.test_results:
	rm -f $@
	echo "main. " | gprolog --query-goal 'consult(main_gprolog)' > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

#     !! abort on Ctrl-<C>; now: segfault -- EOF on pipe
#     !! abort on a compile error
#     !! abort on failure, doc: halt(...) codes
ciao.test_results:
	rm -f $@
	rm -f *.po
	echo "ensure_loaded(main_ciao), main. " | ciao > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

#     !! abort on Ctrl-<C>; now: segfault -- EOF on pipe
#     !! abort on a compile error
#     !! abort on failure, doc: halt(...) codes
yap.test_results:
	rm -f $@
	rm -f *.po
	echo "[main_yap], main. " | yap > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

# vvv Dat: -g makes it abort on Prolog runtime error (-t doesn't)
#     !! abort on Ctrl-<C>
#     !! abort on a compile error
#     !! abort on failure, doc: halt(...) codes
#     !! find the swipl binary, using type etc.
swipl.test_results:
	rm -f $@
	swipl -s main_swipl.pl -g main. < /dev/null > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

# vvv Dat: good news: the aprolog toplevel aborts with 
aprolog.test_results: ../aprolog/aprolog-run ../aprolog/aprolog.bin
	rm -f $@
	echo "consult(main_aprolog), main. " |  ../aprolog/aprolog-run toplevel > $@.tmp
	mv $@.tmp $@
	perl -x ./evaluate.pl test_cases.pl $@ || true

../aprolog/aprolog.bin:
	$(MAKE) -C ../aprolog

clean:
	rm -f *~
	rm -f aprolog.prof
	rm -f bar dave f foo roger_data scowen t tmp.in tmp.out tmpfile.pl

realclean:	clean
	rm -f *.test_results
