append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).

flatten(X, [X]) :-
	atomic(X),
	X \== [],
	!.
flatten([],     []).
flatten([X|Xs], Ys) :-
	flatten(X,  Ys1),
	flatten(Xs, Ys2),
	append(Ys1, Ys2, Ys).
