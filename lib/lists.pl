:- module(lists, [
        nonsingle/1, append/3, reverse/2, reverse/3, delete/3, % member/2, 
	select/3, length/2, nth/3, add_after/4, add_before/4,
        % list/1, list/2, 
	list1/2, dlist/3, list_concat/2, list_insert/2, insert_last/3, 
	%% write_list1/1, Does this belong here? it forces loading write...
        contains_ro/2, contains1/2, nocontainsx/2, last/2, list_lookup/3,
        list_lookup/4,
        intset_insert/3, intset_delete/3, intset_in/2, intset_sequence/3,
	intersection/3, union/3, difference/3, sublist/2, subordlist/2,
	equal_lists/2, list_to_list_of_lists/2, powerset/2
        ],
        [
	assertions,isomodes,metatypes,hiord
	]).

:- comment(title, "List processing").

:- pred nonsingle(X) # "@var{X} is not a singleton.".

:- comment(module,"This module provides a set of predicates for list
           processing.").

nonsingle([_]) :- !, fail.
nonsingle(_).

%%% Now in engine(basic_props)
% :- pred member(X,Xs) # "@var{X} is an element of (list) @var{Xs}.".
% 
% member(X, [X|_]).
% member(X, [_Y|Xs]):- member(X, Xs).

:- pred append(Xs,Ys,Zs)  # "@var{Zs} is @var{Ys} appended to @var{Xs}.".

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

:- pred reverse(Xs,Ys) : list * var => list * list
   # "Reverses the order of elements in @var{Xs}.".

reverse(Xs,Ys):- reverse(Xs,[],Ys).

reverse([], L, L).
reverse([E|Es],L,R) :- reverse(Es,[E|L],R).

:- pred delete(L1,E,L2) # "@var{L2} is @var{L1} without the ocurrences
   of @var{E}.".

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
	delete(Tail, Element, Rest).

:- pred select(X,Xs,Ys) # "@var{Xs} and @var{Ys} have the same
   elements except for one occurrence of @var{X}.".

select(E, [E|Es], Es).
select(E, [X|Es], [X|L]) :- select(E, Es, L).

:- pred length(L,N) : list * var => list * integer
	# "Computes the length of @var{L}.".
:- pred length(L,N) : var * integer => list * integer
	# "Outputs @var{L} of length @var{N}.".
:- pred length(L,N) : list * integer => list * integer
	# "Checks that @var{L} is of length @var{N}.".

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([], I, I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([], I, I) :- !.
dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).

:- comment(nth(N, List, Elem), "@var{N} is the position in @var{List} of
   @var{Elem}.  @var{N} counts from one.").

:- pred nth(+int, ?list, ?term)
        # "Unifies @var{Elem} and the @var{N}th element of @var{List}.".
:- pred nth(-int, ?list, ?term)
        # "Finds the positions where @var{Elem} is in @var{List}.
          Positions are found in ascending order.".

nth(N, List, Elem) :-
        integer(N), !, N >= 1, nthfunc(N, List, Elem).
nth(N, List, Elem) :-
        var(N), !,
        findnth(List, Elem, 1, N).
nth(N, _, _) :-
        throw(error(type_error(integer, N), nth/3-1)).

nthfunc(1, [Elem|_], Elem) :- !.
nthfunc(N, [_|List], Elem) :-
        N1 is N-1,
        nthfunc(N1, List, Elem).

findnth([Elem|_], Elem, N, N).
findnth([_|List], Elem, N0, N) :-
        N1 is N0+1,
        findnth(List, Elem, N1, N).

:- pred add_after(+L0, +E0, +E, -L) # "Adds element @var{E} after
        element @var{E0} (or at end) to list @var{L0} returning in
        @var{L} the new list (uses term comparison).".

add_after([], _, E, [E]).
add_after([E|Es], E0, E1, NEs) :-
        E == E0, !,
        NEs = [E0,E1|Es].
add_after([E|Es], E0, E1, [E|NEs]) :-
        add_after(Es, E0, E1, NEs).

%% add_before(+L0, +E0, +E, -L): adds element E before element E0 (or at
%%     start) to list L0 returning in L the new list (uses term comparison)

add_before(L, E0, E, NL) :-
        add_before_existing(L, E0, E, NL), !.
add_before(L, _, E, [E|L]).

add_before_existing([E|Es], E0, E1, NEs) :-
        E == E0, !,
        NEs = [E1,E0|Es].
add_before_existing([E|Es], E0, E1, NEEs) :-
        add_before_existing(Es, E0, E1, NEs), !,
        NEEs = [E|NEs].

:- prop list1(X,Y)  # "@var{X} is a list of @var{Y}s of at least one element.".
:- meta_regtype(list1/2).

list1([X|R],T) :- 
	T(X),
	list(R,T).

:- pred dlist(List,DList,Tail) # "@var{List} is the result of removing
   @var{Tail} from the end of @var{DList} (makes a difference list from
   a list).".

dlist([], X, X).
dlist([X|Y], [X|L], T) :- dlist(Y, L, T).

:- pred list_concat(list(LL,list),list(L)) # "@var{L} is the
   concatenation of all the lists in @var{LL}.".

list_concat([],[]).
list_concat([L|RL],Head) :- 
	dlist(L,Head,Tail),
	list_concat(RL,Tail).

:- pred list_insert(-List, +Term) # "Adds @var{Term} to the end of
   @var{List} if there is no element in @var{List} identical to
   @var{Term}.".

list_insert(List, Term) :-
	var(List), !,
	List=[Term|_].
list_insert([Term0|_], Term) :-
	Term0==Term, !.
list_insert([_|List], Term) :-
	list_insert(List, Term).

:- pred insert_last(+L0, +E, -L) # "Adds element @var{E} at end of
   list @var{L0} returning @var{L}.".

insert_last(Xs, X, Ys):- append(Xs, [X], Ys).

:- pred contains_ro/2 # "Impure membership (does not instantiate a
   variable in its first argument.".

contains_ro([], _) :- !, fail.
contains_ro([X|_], X).
contains_ro([_|Xs], X) :- contains_ro(Xs, X).

:- pred contains/1 # "First membership.".

contains1([X|_], X) :- !.
contains1([_|Xs], X) :- contains1(Xs, X).

:- pred nocontainsx(L, X) # "@var{X} is not identical to any element
   of @var{L}.".

nocontainsx([], _).
nocontainsx([X1|Xs], X) :- X\==X1, nocontainsx(Xs, X).

:- pred last(L,X) # "X is the last element of L.".

last(L, X) :- var(L), !, L = [X|_].
last([_|L], X) :- last(L, X).

:- pred list_lookup(List, Functor, Key, Value)
        # "Look up @var{Functor}(@var{Key},@var{Value}) pair in variable
   ended key-value pair list @var{L} or else add it at the end.".

list_lookup(List, Functor, Key, Value) :-
	var(List), !,
        functor(Pair, Functor, 2),
        arg(1, Pair, Key),
        arg(2, Pair, Value),
	List=[Pair|_].
list_lookup([Pair|_], Functor, Key, Value) :-
        functor(Pair, Functor, 2),
        arg(1, Pair, Key0),
	Key0==Key, !,
        arg(2, Pair, Value).
list_lookup([_|List], Functor, Key, Value) :-
	list_lookup(List, Functor, Key, Value).

list_lookup(List, Key, Value) :- list_lookup(List, (-), Key, Value).

% intset_... deal with ordered lists of numbers

intset_insert([], A, Set) :- !, Set=[A].
intset_insert(Set0, A, Set) :- Set0=[D|_], A<D, !, Set=[A|Set0].
intset_insert(Set0, D, Set) :- Set0=[D|_], !, Set=Set0.
intset_insert([D|Ds], A, [D|Bs]) :- intset_insert(Ds, A, Bs).

intset_delete([D|Ds], D, Set) :- !, Set=Ds.
intset_delete([D|Ds], A, [D|Ds1]) :- A>D, intset_delete(Ds, A, Ds1).

intset_in(O, [O1|Os]) :-
	(   O1<O -> intset_in(O, Os)
	;   O=O1
	).

intset_sequence(0, L0, L) :- !, L=L0.
intset_sequence(N, L0, L) :- M is N-1, intset_sequence(M, [M|L0], L).

%------------------------------------------------------------------------------
% operations on two lists:

:- pred intersection(+List1,+List2,-List) # "@var{List} has the
        elements which are both in @var{List1} and @var{List2}.".

intersection([], _, []).
intersection([Element|Residue], List, Intersection) :-
	member(Element, List), !, 
	Intersection = [Element|Intersection1],
	intersection(Residue, List, Intersection1).
intersection([_|Residue], List, Intersection) :-
	intersection(Residue, List, Intersection).

:- pred union(+List1, +List2, -List) # "@var{List} has the elements
        which are in @var{List1} followed by the elements which are in
        @var{List2} but not in @var{List1}.".

union([], List2, List2).
union([Element|Residue], List, Union) :-
	member(Element, List), !,
	union(Residue, List, Union).
union([Element|Residue], List, [Element|Union]) :-
	union(Residue, List, Union).

:- pred difference(+List1, +List2, -List) # "@var{List} has the
        elements which are in @var{List1} but not in @var{List2}.".

difference([], _, []) :- !.
difference([Element|Residue], List, Difference) :-
	member(Element, List), !, 
	difference(Residue, List, Difference).
difference([Element|Residue], List, [Element|Difference]) :-
	difference(Residue, List, Difference).

:- prop subordlist(?List1, +List2)
	# "@var{List2} contains all the elements of @var{List1}
	   in the same order.".

subordlist(List, List).
subordlist(Sublist, [H|T]) :- 
	sublist_aux(T, H, Sublist).

sublist_aux(Sublist, _, Sublist).
sublist_aux([H|T], _, Sublist) :- 
	sublist_aux(T, H, Sublist).
sublist_aux([H|T], X, [X|Sublist]) :- 
	sublist_aux(T, H, Sublist).

:- prop sublist(?List1, +List2)
	# "@var{List2} contains all the elements of @var{List1}.".

sublist([], _).
sublist([Element|Residue], List) :-
	member(Element, List),
	sublist(Residue, List).

:- pred equal_lists(+List1, +List2) # "@var{List1} has all the
        elements of @var{List2}, and vice versa.".

equal_lists(List1, List2) :-
	sublist(List1, List2),
	sublist(List2, List1).

:- pred list_to_list_of_lists(+List,-LList) # "@var{LList} is the list
        of one element lists with elements of @var{List}.".

list_to_list_of_lists([X|Xs],[[X]|Xss]) :-
	list_to_list_of_lists(Xs,Xss).
list_to_list_of_lists([],[]).

:- pred powerset(+List,-LList) # "@var{LList} is the powerset of
        @var{List}, i.e., the list of all lists which have elements of
        @var{List}.  If @var{List} is ordered, @var{LList} and all its
        elements are ordered.".

powerset([],[]).
powerset([X|Xs],[[X]|Xss]) :-
	powerset(Xs,Yss),
	add_x(Yss,X,Yss,Xss).

add_x([],_,Zss,Zss).
add_x([Ys|Yss],X,Zss,[[X|Ys]|Xss]) :-
	add_x(Yss,X,Zss,Xss).

% ----------------------------------------------------------------------------

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*5+41,2000/02/04,13:34*24+'CET'), "Split
   @pred{sublist/2} into its two versions --respecting ordering or not
   (Francisco Bueno Carrillo)").

:- comment(version(0*9+63,1999/04/28,16:11*31+'MEST'), "Changed
   list_lookup/4 to allow indexing with functors, following suggestion of
   Per Cederberg. Added nth/3. (Daniel Cabeza Gras)").

:- comment(version(0*9+8,1999/03/18,14:01*40+'MET'), "Taken out
   properties list/1 and list/2 (they are already in basic_props)
   (Daniel Cabeza Gras)").

:- comment(version(0*7+11,1998/09/25,11:30*29+'MEST'), "Added
   @pred{list_concat/2} here.  (Manuel Hermenegildo)").

:- comment(version(0*5+16,1998/06/10,17:55*54+'MET DST'), "Added
   list_lookup/4 to the library.  (Daniel Cabeza Gras)").

:- comment(version(0*4+6,1998/2/25), "Added type list1/2. (Francisco
   Bueno Carrillo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

:- comment(version(0*3+1,1997/12/17), "Unified the two list
   libraries. Added more efficient versions of some
   predicates. (Manuel Hermenegildo)").

:- comment(version(0*3+0,1997/12/16), "Started autodoc. (Manuel
   Hermenegildo)").

% ----------------------------------------------------------------------------
