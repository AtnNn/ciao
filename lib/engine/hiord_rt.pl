:- module(hiord_rt,[call/2 /* call/N, */],[assertions, isomodes]).

:- comment(title,"Higher-order").

:- comment(author,"Daniel Cabeza Gras").

:- comment(module,"This module is a wrapper to the implementation
   defined predicate @pred{call/2}").

:- impl_defined([call/2]).

:- comment(call(Pred,Arg1), "There exists a set of builtin predicates of
   the form @pred{call/N} with @tt{N > 1} which execute predicate
   @var{Pred} given arguments @var{Arg1} ... @var{ArgX}. If @var{Pred}
   has already arguments @var{Arg1} is added to the start, the rest to
   the end. This predicate, when @var{Pred} is a variable, can be
   written using the special Ciao syntax @tt{Pred(Arg1,...,ArgX)}.").

:- true pred call(+callable,?). 

% :- primitive_meta_predicate(call(pred(1),?)).

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+226,2003/12/22,16:47*31+'CET'), "Added comment
   author and version.  (Edison Mera)").

:- comment(version(1*7+146,2001/11/15,19:32*38+'CET'), "Changed from lib
   to engine.  (Daniel Cabeza Gras)").

:- comment(version(1*7+99,2001/05/10,21:06*35+'CEST'), "Changed
   predicate abstractions to use '->' for marking shared variables, and
   to allow put them between @{ and @}.  Operator ` is undefined now.  Thus,
   a valid predicate abstraction wold be @tt{@{M-> _(S) :- mother(M,S)@}}.
   (Daniel Cabeza Gras)").

