p(X) :-
	X=f(A),
	A=a.

q(f(a)).

rr(A) :- r(A), s(A).

r(a).
r(b).
r(c).

s(a).
s(b).
s(c).

t(A, B) :-
	rr(A),
	rr(B),
	neq(A, B).

neq(A, A) :- !, fail.
neq(_, _).

aaa :- call(aaa1).
aaa1 :- call(aaa2).
aaa2 :- call(aaa3).
aaa3 :- call(aaa4).
aaa4.
