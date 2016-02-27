:- module(random, [random/1, random/3, srandom/1], [assertions, isomodes]).

:- comment(title, "Random numbers").
:- comment(author, "Daniel Cabeza").

:- comment(module, "This module provides predicates for generating
        pseudo-random numbers").

:- impl_defined([random/1, random/3, srandom/1]).

:- comment(random(Number), "@var{Number} is a (pseudo-) random number in
        the range [0.0,1.0]").

:- true pred random(-float).

:- comment(random(Low, Up, Number), "@var{Number} is a (pseudo-) random
        number in the range [@var{Low}, @var{Up}]").

:- true pred random(+int,+int,-int)
        # "If @var{Low} and @var{Up} are integers, @var{Number} is an
          integer.".

:- true pred random(+flt,+num,-flt).
:- true pred random(+int,+flt,-flt).

:- comment(srandom(Seed), "Changes the sequence of pseudo-random
        numbers according to @var{Seed}.  The stating sequence of
        numbers generated can be duplicated by calling the predicate
        with @var{Seed} unbound (the sequence depends on the OS).").

:- true pred srandom(?int).



:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+275,2004/01/09,16:25*09+'CET'), "Corrected
Makefile: it tries to chmod only the files just created (permission
errors were being given; same for the sockets Makefile) (Manuel
Carro)").

:- comment(version(1*7+194,2002/04/04,21:08*41+'CEST'), "Corrected an
error in srandom/1 which made it to cause a segmentation violation.
(MCL)").

:- comment(version(1*5+150,2000/05/24,20:00*38+'CEST'), "Changed
Makefile to compile .pl to .po and .itf; otherwise the first load
fails (and, after this, the second succeeds if the user has rights to
write in the directory).

    (MCL)").

:- comment(version(1*5+26,1999/12/29,15:07*59+'CET'), "random/3 made
more random in the case of integer limits. (MCL)").

