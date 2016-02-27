p(a1).
p(a2).

q(a0, b0).
q(A,  B) :-
	p(A),
	p(B).
q(a1, b1).
q(a2, b2).

r([A, B]) :-
	q(A, B).

t(X) :-
	q(X, X).

rtest :-
	r(X),
	writeln(X),
	fail.
rtest.
