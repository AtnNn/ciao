:- module(p1, [m/0,n/1,mm/0]).

:- use_package([asp]).
:- use_asp(asp1, 'asp1.lp').
:- use_asp(asp2, 'asp2.lp').
:- use_asp(asp3, 'asp3.lp').
%:- use_module(p2).

m :-
	display('call asp1:model\n'),
	asp1:model(Q),
	display('call Q:getTrueAtoms(L)\n'),
	Q:getTrueAtoms(L),
	display('call prt(L)\n'),
	prt(L), nl.

mm :- asp1:assert([(q(X) :- r(X), not p(X)), (p(X) :- r(X), not q(X))]),
      display('assertion  complete\n'), read(_),
      asp1:retract([(q(X) :- r(X), not p(X)), (p(X) :- r(X), not q(X))]),
      display('retraction complete\n'), read(_),
	m.

prt([]).
prt([H|T]) :- display(H), display(' '),
	prt(T).

n(1) :- !,findall(Q,asp1:model(Q), L),
	prt_list(L).

n(2) :- asp1:model(Q),
	Q:getTrueAtoms(L),
	display('model: '), prt(L), nl, read(_),
	display('now fail to get all models...\n'),
	fail.
n(2).

n(3) :- n(1), read(_),
	asp2:assert([r(3)]), read(_),
	n(1), read(_).

n(4) :- asp1:compute(0,(a(2), b(2))).
n(41):- asp1:compute(X,L), display('X='),display(X),
	display(' L='), display(L),nl.
n(5) :- asp1:d(X), 
	display('d('), display(X), display(')'), nl, fail.
n(5).

n(6) :- asp2:model(Q),
	Q:getTrueAtoms(L),
	prt(L),nl, read(_),fail.
n(6).

n(7) :- asp2:model(Q),
	Q:getTrueAtoms(L),
	display('Models:'), display(L), nl,
	Q:draw_all_just,
	read(_),
	fail.
n(7).

prt_list([]).
prt_list([H|T]) :-
	H:getTrueAtoms(L),
	prt(L), nl,
	prt_list(T).
