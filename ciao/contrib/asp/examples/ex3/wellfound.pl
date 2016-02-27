:- class(wellfound).

:- use_package([objects]).

:- use_module(library(aggregates)).
:- use_module(library(compiler)).
:- use_module(engine(internals)).
:- data aspInterface/1.
:- data atomClass/1.
:- data symboltableClass/1.
:- data just/2.

:- data flag/2.

:- export(modelNo/1).
:- data modelNo/1.

:- use_class(atomclass).
:- use_class(symboltab).

:- export(getTrueAtoms/1).
:- export(getFalseAtoms/1).
:- export(unknown/1).
:- export(data_add/1).
:- export(atomClass/1).
:- export(symboltableClass/1).
:- export(just/2).
:- export(atom_val/2).
:- export(getAtomNo/2).

wellfound(X) :- assertz_fact(aspInterface(X)).

atom_val(Atom,V) :- 
	getAtomNo(Atom,AtomNo),
	just(AtomNo,V).

unknown(Atom) :- getAtomNo(Atom,AtomNo),
         \+just(AtomNo,_).

getAtomNo(Atom,No):- atomClass(A),A:atom(No,Atom).

getTrueAtoms(L) :- var(L),
    atomClass(A),
    findall(Atom,(just(No,1),A:atom(No,Atom),Atom=.. [P|_],\+atom_concat(aspprolog,_,P)),L).

getFalseAtoms(L) :- var(L),
    atomClass(A),
findall(Atom,(just(No,0),A:atom(No,Atom),Atom=.. [P|_],\+atom_concat(aspprolog,_,P)),L).

data_add(atomClass(A)) :- assertz_fact(atomClass(A)).
data_add(just(No,V)) :- assertz_fact(just(No,V)).
data_add(symboltableClass(ST)) :-
    assertz_fact(symboltableClass(ST)).

