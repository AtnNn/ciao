:- class(atomclass).

:- use_package([objects]).

:- export(atom/2).
:- export(addAtom/2).
:- export(incRef/0).
:- export(decRef/0).

:- data atom/2.
:- data ref/1.

ref(0).
incRef :- ref(C),!,retractall_fact(ref(_)),
	C1 is C+1,
	assertz_fact(ref(C1)).
incRef :- assertz_fact(ref(0)).
decRef :- ref(C), retractall_fact(ref(_)),
	C1 is C-1,
	assertz_fact(ref(C1)),
	(C1=0 -> self(SELF),destroy(SELF);true).

addAtom(Index,AtomName) :-
	assertz_fact(atom(Index,AtomName)).

