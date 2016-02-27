evalpol([],    X, 0).
evalpol([C|L], X, R) :-
	evalpol(L, X, R0),
	R1 is R0 * X,
	R is R1 + C.
