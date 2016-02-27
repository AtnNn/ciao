tune_profiler(N, M, R) :-
	diff_time(N, M, D),
	count(N, C),
	R is D / C.

min_time_loop(0, N, T,  T) :- !.
min_time_loop(M, N, T0, T) :-
	M > 0,
	time_loop(N, T1),
	sel_min(T0, T1, T2),
	M1 is M - 1,
	min_time_loop(M1, N, T2, T).

sel_min(T0, T1, T0) :-
	T0 < T1,
	!.
sel_min(T0, T1, T1).

diff_time(N, M, D) :-
	profile('/tmp/tune_profiler', min_time_loop(M, N, 1000000000, TimeProf)
	),
	call(min_time_loop(M, N, 1000000000, TimeActual)),
	D is TimeProf -TimeActual.

count(N, C) :-
	C0 is 16 * N,
	C is 33 + C0.

dummy_loop(0) :- !.
dummy_loop(N) :-
	N > 0,
	N1 is N - 1,
	dummy_loop(N1).

time_loop(N, T) :-
	hrtime(T1U, T1L),
	dummy_loop(N),
	hrtime(T2U, T2L),
	sub2(T2U, T2L, T1U, T1L, _TU, T).
