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

:- true prop ground(@X) => gnd(X) + native
	# "@var{X} is currently ground (it contains no variables).".

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

:- true prop atom(@X)=> atm(X) + native
	# "@var{X} is currently instantiated to an atom.".

atom(X) :- atom(X).

:- true prop atomic(@X) + native
	# "@var{X} is currently instantiated to an atom or a number.".

atomic(X) :- atomic(X).

:- true prop float(@X) => flt(X) + native
	# "@var{X} is currently instantiated to a float.".

float(X) :- float(X).

:- true prop integer(@X) => int(X) + native
	# "@var{X} is currently instantiated to an integer.".

integer(X) :- integer(X).

:- true comp nonvar(X) + ( native, native(not_free(X)) ).
:- true prop nonvar(@X)
   # "@var{X} is currently a term which is not a free variable.".

nonvar(X) :- nonvar(X).

:- true prop number(@X) => num(X) + native
	# "@var{X} is currently instantiated to a number.".

number(X) :- number(X).

:- true comp var(X) + ( native, native(free(X)), sideff(hard) ).
:- true prop var(@X) # "@var{X} is a free variable.".

var(X) :- var(X).

:- true prop type(X,Y) => atm(Y) + native # "@var{X} is internally of type
   @var{Y} (@tt{var}, @tt{attv}, @tt{float}, @tt{integer},
   @tt{structure}, @tt{atom} or @tt{list}).".

type(X, Y) :- type(X, Y). 

:- comment(version_maintenance,dir('../../version')).

:- comment(version(0*7+8,1998/09/23,19:21*44+'MEST'), "Changed
   assertion comment to #.  (Manuel Hermenegildo)").


