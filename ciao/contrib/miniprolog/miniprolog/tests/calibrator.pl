calib(0) :- !.
calib(N) :-
	N < 0,
	!,
	N1 is N + 1,
	calib(N1),
	calib(N1).
calib(N) :-
	N >= 1,
	N \== 0,
	N1 is N - 1,
	N2 is N + 1,
	N3 is N1 * N2,
	N4 is N1 / N2,
	calib(N1),
	calib(N1).

calib2(0) :- !.
calib2(N) :-
	0 > N,
	!,
	N1 is N + 1,
	calib2(N1),
	calib2(N1).
calib2(N) :-
	1 =< N,
	N \== 0,
	N1 is N - 1,
	N2 is N + 1,
	N3 is N1 * N2,
	N4 is N1 / N2,
	p(a),
	p(1),
	calib2(N1),
	calib2(N1).

p(a).
p(1).

tstruct([], _, []) :- !.
tstruct(A,  X, A) :- X = b, !.
tstruct(A,  X, A) :- X = c, !.

tstruct([X|Xs], A, [f(h(A, A, 1, a, 2), g(A, b, c), i(X, 3))|Ys]) :-
	tstruct(Xs,  A, Ys0),
	tstruct(Ys0, A, Ys).

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([],    I,  I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([],    I,  I) :- !.
dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).

reverse(L, R) :-
	reverse2(L, [], R).

reverse2([],    R,  R).
reverse2([E|L], R0, R) :-
	reverse2(L, [E|R0], R).

compare([], [], []) :-
	!.
compare([X|Xs], [X|Ys], [1|Zs]) :-
	!,
	compare(Ys, Xs, Zs),
	compare(Xs, Ys, Zs).
compare([X|Xs], [Y|Ys], [0|Zs]) :-
	compare(Ys, Xs, Zs),
	compare(Xs, Ys, Zs).

calibrate(A, B, C, D, N, E, Y) :-
	calib_unequal,
	calib(D),
	calib2(D),
	tstruct(A, E, _X),
	dlength(L, 0, N),
	reverse(L, R),
	reverse(R, S),
	compare(B, C, Y).

calib_unequal :-
	calib_unequal_1_10,
	calib_unequal_1_10,
	calib_unequal_1_10,
	calib_unequal_1_10,
	calib_unequal_1_10,

	calib_unequal_1_10,
	calib_unequal_1_10,
	calib_unequal_1_10,
	calib_unequal_1_10,
	calib_unequal_1_10.

calib_unequal_1_10 :-
	compare_calib_unequal_1_10(1, 1).
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B) :- A \== B.
compare_calib_unequal_1_10(A, B).
