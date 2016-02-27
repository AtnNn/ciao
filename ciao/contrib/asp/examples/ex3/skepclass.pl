:- class(skepclass).

:- use_package([objects]).
:- use_module(library(aggregates)).

:- export(addSkep/2).
:- export(incRef/0).
:- export(decRef/0).
:- export(saveState/1).
:- export(addClasses/1).
:- export(getAllSkepTrue/1).
:- export(symboltableClass/1).
 :- data symboltableClass/1.

:- data just/2, state/1.
:- data ref/1.
:- export(atomClass/1).
:- data atomClass/1, ruleClass/1.

:- export(just/2).

ref(0).
incRef :- ref(C),retractall_fact(ref(_)),
	C1 is C+1,
	assertz_fact(ref(C1)).
incRef :- assertz_fact(ref(0)).
decRef :- ref(C),retractall_fact(ref(_)),
	C1 is C-1,
	assertz_fact(ref(C1)),
	(C1=0 -> (self(SELF),destroy(SELF));true).
saveState(State) :- state(State),!.
saveState(State) :- assertz_fact(state(State)).

addClasses(A) :- assertz_fact(atomClass(A)).
:- export(data_add/1).
data_add(symboltableClass(ST)):-
	assertz_fact(symboltableClass(ST)).

addSkep([],_).
addSkep([H|T],V) :-
	atomClass(A),
	A:atom(I,H),
	assertz_fact(just(I,V)),
	addSkep(T,V).

:- export(getAtomNo/2).
getAtomNo(Atom,No):- atomClass(A),
	A:atom(No,Atom).

:- export(prtAtom/0).
prtAtom :- atomClass(A),
	A:atom(No,Atom),
	display(Atom), display(' '), display(No),nl,
	fail.
prtAtom.

:- export(getSkep/2).

getSkep(Atom,V) :- !,
	atomClass(A),
	A:atom(I,Atom), just(I,V).

getAllSkepTrue(Atoms) :- !,
	atomClass(A),
	findall(N,(just(I,1),A:atom(I,N)),Atoms).

destructor :-
    retractall_fact(just(_,_)),
    atomClass(A),
    A:decRef,
    retractall_fact(atomClass(_)),
    symboltableClass(ST),
    ST:decRef,
    retractall_fact(symboltableClass(_)).
