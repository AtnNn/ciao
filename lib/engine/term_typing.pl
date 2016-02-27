:- module(term_typing, [
        var/1, nonvar/1, atom/1, integer/1, float/1, number/1, atomic/1,
        ground/1, type/2],
        [assertions, isomodes]).

:- comment(title,"Extra-logical properties for typing").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This library contains traditional Prolog predicates
        for testing types.  They depend on the state of instantiation of
        their arguments, thus being of extra-logical nature.").

:- true prop ground(X) + native
	# "@var{X} is currently ground (it contains no variables).".
:- true pred ground(@X) => gnd(X) + native.

ground(Term):-
	nonvar(Term),
	functor(Term,_,N),
	ground_(N,Term).

ground_(0,_) :- !.
ground_(N,Term):-
	N > 0,
	arg(N,Term,Arg),
	ground(Arg),
	N1 is N-1,
	ground_(N1,Term).

% Compiled inline -- these are hooks for the interpreter.

:- true prop atom(X) + native
	# "@var{X} is currently instantiated to an atom.".
:- true pred atom(@X) => atm(X) + native.

atom(X) :- atom(X).

:- true prop atomic(X) + native
	# "@var{X} is currently instantiated to an atom or a number.".
:- true pred atomic(@X) + native.

atomic(X) :- atomic(X).

:- true prop float(X) + native
	# "@var{X} is currently instantiated to a float.".
:- true pred float(@X) => flt(X) + native.

float(X) :- float(X).

:- true prop integer(X) + native
	# "@var{X} is currently instantiated to an integer.".
:- true pred integer(@X) => int(X) + native.

integer(X) :- integer(X).

:- true prop nonvar(X) + native(not_free(X))
   # "@var{X} is currently a term which is not a free variable.".
:- true pred nonvar(@X) + native.

nonvar(X) :- nonvar(X).

:- true prop number(X) + native
	# "@var{X} is currently instantiated to a number.".
:- true prop number(@X) => num(X) + native.

number(X) :- number(X).

:- true prop var(X) + native(free(X))
   # "@var{X} is a free variable.".
:- true pred var(@X) + ( native, sideff(hard) ).

var(X) :- var(X).

:- true prop type(X,Y) + native
   # "@var{X} is internally of type @var{Y} (@tt{var}, @tt{attv}, @tt{float},
      @tt{integer}, @tt{structure}, @tt{atom} or @tt{list}).".
:- true pred type(X,Y) => atm(Y) + native.

type(X, Y) :- type(X, Y). 

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+124,2003/12/26,20:06*44+'CET'), "Changed the
   :- prop with properties to :- pred.  (Francisco Bueno Carrillo)").

:- comment(version(0*7+8,1998/09/23,19:21*44+'MEST'), "Changed
   assertion comment to #.  (Manuel Hermenegildo)").


