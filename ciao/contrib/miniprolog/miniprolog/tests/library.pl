reverse([],     X, X).
reverse([X|Xs], Y, Z) :-
	reverse(Xs, [X|Y], Z).

append([],     X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs, Y, Zs).

length(X, N) :-
	length2(X, 0, N).

length2([],    N,  N) :- !.
length2([X|L], N0, N) :-
	N1 is N0 + 1,
	length2(L, N1, N).

nreverse([],     []) :- !.
nreverse([X|Xs], Y) :-
	nreverse(Xs, Y0),
	append(Y0, [X], Y).
