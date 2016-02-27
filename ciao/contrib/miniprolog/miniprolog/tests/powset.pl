powset([],    [[]]).
powset([X|L], P) :-
	powset(L, P0),
	append_elem(P0, X, P, P0).

append_elem([],     X, T,          T).
append_elem([L|Ls], X, [[X|L]|Rs], T) :-
	append_elem(Ls, X, Rs, T).
