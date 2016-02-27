:- class(justclass).

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
:- export(atom_val/1).
:- export(getAtomNo/2).

justclass(X,No) :- assertz_fact(aspInterface(X)),
	assertz_fact(modelNo(No)).

atom_val(Atom) :- \+ground(Atom), !,
        getAtomNo(Atom,AtomNo), 
         just(AtomNo, 1).
atom_val(Atom) :- ground(Atom),
        getAtomNo(Atom,AtomNo), 
        just(AtomNo,1),!.
atom_val(Atom) :- ground(Atom),
        getAtomNo(Atom,AtomNo), 
         \+just(AtomNo,0),
         !, loop_forever.
loop_forever :- loop_forever.

unknown(Atom) :- getAtomNo(Atom,AtomNo),
         \+just(AtomNo,_), loop_forever.

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

graph_just_atoms(AtomList, G) :- 
         self(Name),
    aspInterface(IntName),
    use_module(IntName),
    module_concat(IntName,remFM,G2),
    '$meta_call'(G2),
    module_concat(IntName,gen_graph(Name,AtomList,G),G1),
    '$meta_call'(G1).

graph_just_all(G):-
    self(Name),
    atomClass(A),
    findall(Atom, A:atom(_,Atom), AtomList),
    aspInterface(IntName),
    use_module(IntName),
    module_concat(IntName,remFM,G2),
    '$meta_call'(G2),
    module_concat(IntName,gen_graph(Name,AtomList,G),G1),
    '$meta_call'(G1).

draw_all_just :- 
     self(Name),    aspInterface(IntName),
    use_module(IntName),
    module_concat(IntName,remFM,G2),
    '$meta_call'(G2),
    module_concat(IntName,justify_all(Name),G1),
    '$meta_call'(G1).

draw_atom_just(A) :- 
    self(Name),    aspInterface(IntName),
    use_module(IntName),
    module_concat(IntName,remFM,G2),
    '$meta_call'(G2),
    module_concat(IntName,justify_atoms(A,Name),G1),
    '$meta_call'(G1).

:- export(modJust/3).

modJust(B,S1,S2) :- self(Q),
    aspInterface(IntName),
    use_module(IntName),
     modelNo(No),
    module_concat(IntName,incFM(IntName,No),G2),
    '$meta_call'(G2),
      module_concat(IntName,addHideFlag,G4),
      '$meta_call'(G4),
    module_concat(IntName,jjjj(Q,B,S1,S2,0),G1),
    '$meta_call'(G1).

destructor :-
    retractall_fact(just(_,_)),
    atomClass(A),
    A:decRef,
    retractall_fact(atomClass(_)),
    symboltableClass(ST),
    ST:decRef,
    retractall_fact(symboltableClass(_)).

