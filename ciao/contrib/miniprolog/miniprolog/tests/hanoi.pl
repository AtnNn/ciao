elem(a).
elem(b).
elem(c).

append([],     L,  L).
append([X|L1], L2, [X|L3]) :-
	append(L1, L2, L3).

hanoi(1, A, _, C, [mv(A, C)]) :- !.
hanoi(N, A, B, C, M) :-
	N1 is N - 1,
	hanoi(N1, A, C, B, M1),
	hanoi(N1, B, A, C, M2),
	append(M1, [mv(A, C)], T),
	append(T,  M2,         M).
