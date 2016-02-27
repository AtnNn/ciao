:- module(hiord, [map/3,foldl/4], [assertions,functions]).

:- comment(title,"Some Higher-Order Predicates").

:- comment(author,"Daniel Cabeza").

:- comment(module,"This library implements a few basic higher-order
   predicates. These add functionality to the basic 
   higher-order functionality of Ciao. Examples of the latter are:


   Using pred(1):

@begin{verbatim}
  list(L, between(1,6))
  list(L, functor(_,2))
  list(L, >(0))
@end{verbatim}

   Using pred(2):

").

:- pred map(LList,Op,RList) # "Examples of use:
@begin{verbatim}
  map([1,3,2], arg(f(a,b,c,d)), [a,c,b]) or
  map([1,3,2], nth([a,b,c,d]), [a,c,b])
  map([\"D\",\"C\"], append(\".\"), [\"D.\",\"C.\"])
@end{verbatim}
".

:- meta_predicate map(_,pred(2),_).

map([], _) := [].
map([X|Xs], P) := [~P(X)|~map(Xs,P)].

:- pred foldl(List,Seed,Op,Result) # "Example of use:
@begin{verbatim}
?- foldl([\"daniel\",\"cabeza\",\"gras\"], \"\", 
         (\:(X,Y,Z) :- append(X, \" \"||Y, Z)), R).

R = \"daniel cabeza gras \" ? 
@end{verbatim}
".

:- meta_predicate foldl(_,_,pred(3),_).

foldl([], Seed, _Op) := Seed.
foldl([X|Xs], Seed, Op) := ~Op(X,~foldl(Xs,Seed,Op)).

% ---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*5+65,2000/03/15,22:09*28+'CET'), "Added some
   minimal documentation to @lib{hiord} library.  (Manuel Hermenegildo)").

:- comment(version(1*5+64,2000/03/15,22:07*53+'CET'), "Added a minimal
   first version of @lib{hiord} library.  (Daniel Cabeza Gras)").
% ---------------------------------------------------------------------------

