:- class(atomclass).

:- export(atom/2).
:- export(addAtom/2).
:- export(incRef/1).

:- data atom/1.
:- data ref/1.

ref(1).
incRef :- retractall(ref(C)),
	C1 is C+1,
	assertz_fact(ref(C1)).
decRef :- retractall(ref(C)),
	C1 is C-1,
	assertz_fact(ref(C1)),
	(C1=0 -> destroy(self);true).

addAtom(Index,AtomName) :-
	assertz_fact(atom(Index,AtomName).
