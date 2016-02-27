:- module(peano, [
		peano_num/1,
		add/3,
		mul/3
	    ], [
		codecoverage
	    ]).

peano_num(0).
peano_num(s(A)) :- peano_num(A).

add(0,    B, B) :- peano_num(B).
add(s(A), B, s(C)) :- add(A, B, C).

mul(A, B, 0) :-
	( A = 0 -> peano_num(B) ;
	    (B = 0 -> peano_num(A)) ).
mul(s(A), B, C) :-
	mul(A, B, C1),
	add(B, C1, C).
