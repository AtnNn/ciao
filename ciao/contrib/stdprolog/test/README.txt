Prolog ISO conformance testing (framework and test cases)
maintained by pts@fazekas.hu at Sat Jan 14 16:01:27 CET 2006

Before runnig the tests you need:

-- a UNIX system, preferably Linux
-- a Bourne-comatible /bin/sh
-- GNU Make as `make'
-- Perl 5.x
-- a Prolog implementation you want to test. Currently supported:
   SICStus, SWI-Prolog and aprolog. (aprolog is included in the CVS tree.)
-- GCC if you want to compile aprolog

Run the tests:

  $ make sicstus.test_results
  $ make swipl.test_results
  $ make aprolog.test_results

Please note that the aprolog test runs significantly slower than the others,
and it also needs a lot of 

See the *.test_results file for a detailed test results.

To get a summary of the test results, run:

  $ perl -x evaluate.pl test_results.pl <ANY.test_results>

If you want to test a Prolog implementation not supported yet, for example
`foopl', you have to:

-- install foopl to your computer
-- add the `foopl.test_result:' clause to the Makefile
-- write main_foopl.pl
-- write utils_foopl.pl
-- run `make foopl.test_result', and fix the problems

---

gprolog 1.2.16, 2002.

doesn't compile with
  gcc version 3.3.5  (Gentoo Linux 3.3.5-r1, ssp-3.3.2-3, pie-8.7.7.1)
  !! pl2wam
compiles OK with gcc version 2.95.4 20011002 (Debian prerelease)

---

ciao-1.10p6

http://www.clip.dia.fi.upm.es/~clip/Software/Ciao/ciao-1.10p6.tar.gz
SUXX: make and make install recompile all
SUXX: ciao
      zsh: segmentation fault  ciao

--- yap 5.0.1

http://www.ncc.up.pt/~vsc/Yap/current/Yap-5.0.1-static.tar.gz


__END__
