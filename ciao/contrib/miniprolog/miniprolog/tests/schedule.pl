schedule(MinTime) :-
	findall(G, legal_schedule(_A, _B, _C, _D, _E, _F, G), Time),
	earliest_completion(Time, MinTime).

legal_schedule(A, B, C, D, E, F, G) :-
	generator(7, 10, [A, B, C, D, E, F, G]),
	schedule_constraints(A, B, C, D, E, F, G).

schedule_constraints(A, B, C, D, E, F, G) :-
	precedence_constraints(A, B, C, D, E, F, G),
	distance_constraints(B, C, D).

precedence_constraints(A, B, C, D, E, F, G) :-
	A1 is A + 1,
	B >= A1,
	C >= A1,
	D >= A1,
	B1 is B + 5,
	E >= B1,
	C1 is C + 3,
	E >= C1,
	D1 is D + 5,
	F >= D1,
	E1 is E + 2,
	F >= E1,
	F1 is F + 1,
	G >= F1.

distance_constraints(B,  C, _D) :- C >= B + 1.
distance_constraints(_B, C, D) :- D >= C + 1.

generator(0, _, []).
generator(M, N, [Q|L]) :-
	M > 0,
	choose(N, Q),
	M1 is M -1,
	generator(M1, N, L).

choose(N, N) :- N > 0.
choose(N, M) :- N > 0, N1 is N -1, choose(N1, M).

earliest_completion([],       10000).
earliest_completion([T|Time], MinTime) :-
	earliest_completion(Time, MTime),
	min(T, MTime, MinTime).

min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.
