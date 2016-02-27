:- module(between, [between/3], [assertions, isomodes]).

:- comment(title, "Enumeration of integers inside a range").

:- comment(author, "The CLIP Group.").

:- comment(module, "This modules enumerates integers between two
numbers, or checks that an integer lies within a range").

:- comment(summary, "This modules enumerates integers between two
numbers, or checks that an integer lies within a range.  If the second
purpose is the needed one, it is probably wiser (faster and clearer)
to check in the program itself using directly arithmetic
predicates.").

:- pred between(+Min, +Max, ?N) : number * number * int # "@var{N} is
an integer which is greater than or equal to @var{Min} and smaller
than or equal to @var{Max}.  Both @var{Min} and @var{Max} can be
either integer or real numbers.".

between(Min, Max, N) :- integer(N), !, N >= Min, N =<  Max.
between(Min, Max, V) :- var(V), Min =< Max, between_nd(V, Min, Max).

between_nd(Min, Min, _).
between_nd(N, Min, Max) :-
        Min < Max, NMin is Min+1,
        between_nd(N, NMin, Max).

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*9+276,2004/01/09,16:37*11+'CET'), "Changed
   argument order of @pred{between/3} to match original definition by
   O'Keefe (Daniel Cabeza & Jose Morales)").

