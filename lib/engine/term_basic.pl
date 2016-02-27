:- module(term_basic, [
        (=)/2, arg/3, functor/3, (=..)/2, copy_term/2, 'C'/3],
        [assertions, isomodes]).

:- comment(title,"Basic term manipulation").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This module provides basic term manipulation.").

:- true pred copy_term(Term, Copy) + ( sideff(free), native, iso )

        # "@var{Copy} is a renaming of @var{Term}, such that brand new
           variables have been substituted for all variables in
           @var{Term}.  If any of the variables of @var{Term} have
           @concept{attributes}, the copied variables will have copies
           of the attributes as well. It behaves as if defined by:

@begin{verbatim}
:- data 'copy of'/1.

copy_term(X, Y) :-
        asserta_fact('copy of'(X)),
        retract_fact('copy of'(Y)).
@end{verbatim}".

:- impl_defined(copy_term/2).

% Compiled inline -- these provide hooks for the interpreter and comments.

:- true pred '='(?X,?Y) + ( sideff(free), native, iso , eval)
	# "@var{X} and @var{Y} unify.".

X=Y :- X=Y.

:- true pred arg(+ArgNo,+Term,?Arg)
	: int(ArgNo) + ( sideff(free), native, iso, eval ) 
	# "Argument @var{ArgNo} of the term @var{Term} is @var{Arg}.".

arg(X, Y, Z) :- arg(X, Y, Z).

:- true pred functor(?Term,?Name,?Arity) => ( atm(Name), num(Arity) )
	+ ( sideff(free), native, iso )
        # "The principal functor of the  term @var{Term} has name @var{Name}
           and arity @var{Arity}.".
:- true pred functor(+Term,?Name,?Arity) + eval.
:- true pred functor(?Term,+Name,+Arity) + eval.

functor(X, Y, Z) :- functor(X, Y, Z).

:- true pred (?Term =.. ?List) => list(List) + ( sideff(free), native, iso )
	# "The functor and arguments of the term @var{Term} comprise the 
           list @var{List}.".
:- true pred (+Term =.. ?List)  + eval.
:- true pred (_ =.. List) : (list(List),atom_head(List)) + eval.

X=..Y :- X=..Y.

:- prop atom_head/1.

atom_head([Head|_]):-
	atom(Head).

:- true pred 'C'(?S1,?Terminal,?S2) + ( sideff(free), native )
	# "@var{S1} is connected by the terminal @var{Terminal} to @var{S2}.
           Internally used in @em{DCG grammar rules}. Defined as if by the 
           single clause: @tt{'C'([X|S], X, S).}".

'C'(X, Y, Z) :- 'C'(X, Y, Z).

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+170,2004/02/03,21:41*49+'CET'), "Added sideff
   declarations.  (Francisco Bueno Carrillo)").

:- comment(version(1*11+68,2003/12/19,16:34*14+'CET'), "Added comment
   module.  (Edison Mera)").

