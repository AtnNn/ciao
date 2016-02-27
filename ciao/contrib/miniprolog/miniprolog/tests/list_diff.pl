list_diff([],     L,  []).
list_diff([H|L1], L2, L3) :-
	memberchk(H, L2),
	!,
	list_diff(L1, L2, L3).
list_diff([H|L1], L2, [H|L3]) :-
	list_diff(L1, L2, L3).

memberchk(X, [X|L]) :- !.
memberchk(X, [A|L]) :- memberchk(X, L).
