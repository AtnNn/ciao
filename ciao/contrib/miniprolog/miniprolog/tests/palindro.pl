palindro([],         []).
palindro([First|L1], L2) :-
	palindro(L1, Ls2),
	palindro(L1, Lg2),
	append(Ls2, [First|Lg2], L2).

append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).
