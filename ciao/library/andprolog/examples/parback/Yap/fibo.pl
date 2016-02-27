:- module(fibo,
	[
	    speedups/1
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ejecuta(X,N) :- N =< 1, !, fib_seq(X,_).
ejecuta(X,N) :- 
	fib_seq(X,_),
	N1 is N - 1,
	ejecuta(X,N1).

main_seq(X,N) :-
        statistics(walltime, [_,_]),
	ejecuta(X,N),
        statistics(walltime, [_,T2]),
	T is (T2 / N),
	display(time(T)),nl.

speedups(N) :-
	main_seq(25,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fib_seq(0, 0) :- !.
fib_seq(1, 1) :- !.
fib_seq(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib_seq(N1, F1),
        fib_seq(N2, F2),
        F is F1 + F2.

