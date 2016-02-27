:- module(aspQualify, [chkQualify/2]).

mtype(Q,0) :- var(Q),!.
mtype(Q,1) :- functor(Q,justclass,1),!.
mtype(Q,1) :- functor(Q,wellfound,1),!.
justTerm(getTrueAtoms(_),0) :- !.
justTerm(getFalseAtoms(_),0) :- !.
justTerm(unknown(_),0) :- !.
justTerm(draw_all_just,0) :- !.
justTerm(draw_atom_just(_),0) :- !.
justTerm(graph_just_all(_),0) :- !.
justTerm(graph_just_atom(_,_),0) :- !.
justTerm(_,1).

doQualify(0,C1,C2) :- !,C1:C2.
doQualify(1,C1,not(C2)) :- +C1:atom_val(C2).
doQualify(1,C1,C2) :- !,justTerm(C2,J),(J=0->C1:C2;C1:atom_val(C2)).
chkQualify(C1,C2) :- !,
	mtype(C1,T),
	doQualify(T,C1,C2).

