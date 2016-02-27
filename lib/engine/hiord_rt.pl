:- module(hiord_rt, 
	[
	    call/1,
	    call/2 /* call/N, */, 
	    'SYSCALL'/1, 
	    '$nodebug_call'/1,
	    '$meta_call'/1
	],[assertions, isomodes]).

:- comment(title,"Higher-order").

:- comment(author,"Daniel Cabeza Gras").

:- comment(module,"This module is a wrapper to the implementation
   defined predicate @pred{call/1}, and implements the @pred{call/2}
   predicate.").

:- comment(call(G), "Executes goal @var{G}, restricting the scope of
   the cuts to the execution of @var{G}.  Equivalent to writing a
   variable @var{G} in a goal position.").

:- true pred call(+callable) + (iso, native). 
:- primitive_meta_predicate(call(goal)).
:- impl_defined([call/1]).

:- comment(call(Pred,Arg1), "There exists a set of builtin predicates
   of the form @pred{call/N} with @tt{N > 1} which execute predicate
   @var{Pred} given arguments @var{Arg1} ... @var{ArgX}. If @var{Pred}
   has already arguments @var{Arg1} is added to the start, the rest to
   the end. This predicate, when @var{Pred} is a variable, can be
   written using the special Ciao syntax @tt{Pred(Arg1,...,ArgX)}.").

:- true pred call(+callable,?) + native. 
% :- primitive_meta_predicate(call(pred(1),?)).

call(V, Args) :- calln(V, Args).

calln(V, _) :- var(V), !, throw(error(instantiation_error, call/n-1)).
calln(Pred, Args) :-
        Pred = 'PA'(Sh,_H,_B),
        copy_term(Pred, 'PA'(Sh,Args,Goal)), !,
        '$meta_call'(Goal).
calln(Pred, Args) :-
        Pred = 'PA'(_Sh,H,_B),
        functor(H,'',N),
        functor(Args,_,N), !, % Predicate abstraction OK, argument unif. failed
        fail.
calln(Pred, Args) :-
        functor(Args,_,N),
        throw(error(type_error(pred(N),Pred), call/n-1)).

:- impl_defined(['SYSCALL'/1]).

:- primitive_meta_predicate('$nodebug_call'(goal)).
:- impl_defined(['$nodebug_call'/1]).

:- impl_defined(['$meta_call'/1]).


:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+102,2003/12/22,16:52*51+'CET'), "Added comment
   author and version.  (Edison Mera)").

:- comment(version(1*11+5,2003/04/07,13:54*05+'CEST'), "Included
   low-level call predicates previously defined in internals.pl.
   (Jose Morales)").

:- comment(version(1*7+146,2001/11/15,19:32*38+'CET'), "Changed from lib
   to engine.  (Daniel Cabeza Gras)").

:- comment(version(1*7+99,2001/05/10,21:06*35+'CEST'), "Changed
   predicate abstractions to use '->' for marking shared variables, and
   to allow put them between @{ and @}.  Operator ` is undefined now.  Thus,
   a valid predicate abstraction wold be @tt{@{M-> _(S) :- mother(M,S)@}}.
   (Daniel Cabeza Gras)").

