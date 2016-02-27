list_inters([],     _L, []).
list_inters([H|L1], L2, [H|L3]) :-
	memberchk(H, L2),
	!,
	list_inters(L1, L2, L3).
list_inters([_H|L1], L2, L3) :-
	list_inters(L1, L2, L3).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :- memberchk(X, L).
