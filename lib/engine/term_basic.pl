:- module(term_basic, [
        (=)/2, arg/3, functor/3, (=..)/2, copy_term/2, 'C'/3],
        [assertions, isomodes]).

:- comment(title,"Basic term manipulation").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- true pred copy_term(Term, Copy) + iso

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

:- true prop '='(?X,?Y) + iso # "Unify @var{X} and @var{Y}.".

X=Y :- X=Y.

:- true pred arg(+ArgNo,+Term,?Arg) : integer(ArgNo) + iso # "Argument
   @var{ArgNo} of the term @var{Term} is @var{Arg}.".

arg(X, Y, Z) :- arg(X, Y, Z).

:- true pred functor(?Term,?Name,?Arity) + iso # "The principal functor of the
   term @var{Term} has name @var{Name} and arity @var{Arity}.".

functor(X, Y, Z) :- functor(X, Y, Z).

:- true pred (?Term =.. ?List) + iso # "The functor and arguments of the term
   @var{Term} comprise the list @var{List}.".

X=..Y :- X=..Y.

:- true pred 'C'(?S1,?Terminal,?S2) # "@var{S1} is connected by the
   terminal @var{Terminal} to @var{S2}. Used in @em{DCG grammar
   rules}. Defined as if by the single clause: @tt{'C'([X|S], X, S).}
".

'C'(X, Y, Z) :- 'C'(X, Y, Z).

:- comment(version_maintenance,dir('../../version')).

