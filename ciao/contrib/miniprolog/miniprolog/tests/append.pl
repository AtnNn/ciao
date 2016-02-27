append([],     X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs, Y, Zs).
