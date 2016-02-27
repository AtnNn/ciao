append([],     X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs, Y, Zs).

nreverse([],     []).
nreverse([X|Xs], Y) :-
	nreverse(Xs, Y0),
	append(Y0, [X], Y).
