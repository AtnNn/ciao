:- module(random, [random/1, random/3, srandom/1], [assertions, isomodes, foreign_interface]).

:- comment(title, "Random numbers").
:- comment(author, "Daniel Cabeza").

:- comment(module, "This module provides predicates for generating
        pseudo-random numbers").

:- impl_defined([random/1, random/3, srandom/1]).

:- comment(random(Number), "@var{Number} is a (pseudo-) random number in
        the range [0.0,1.0]").

:- true pred random(-float) + native.

:- comment(random(Low, Up, Number), "@var{Number} is a (pseudo-) random
        number in the range [@var{Low}, @var{Up}]").

:- true pred random(+int,+int,-int) + native(random3)
        # "If @var{Low} and @var{Up} are integers, @var{Number} is an
          integer.".

:- true pred random(+flt,+num,-flt).
:- true pred random(+int,+flt,-flt).

:- comment(srandom(Seed), "Changes the sequence of pseudo-random
        numbers according to @var{Seed}.  The stating sequence of
        numbers generated can be duplicated by calling the predicate
        with @var{Seed} unbound (the sequence depends on the OS).").

:- true pred srandom(+int) + native.

:- use_foreign_source(random).

:- comment(version_maintenance,dir('../../version')).

%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*5+26,1999/12/29,15:07*59+'CET'), "random/3 made
more random in the case of integer limits. (MCL)").
