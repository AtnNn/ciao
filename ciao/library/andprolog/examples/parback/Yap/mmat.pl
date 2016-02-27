:- module(mmat,
	[
	    speedups/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ejecuta(X,Y,N) :- N =< 1, !, mmatrix_seq(X,Y,_).
ejecuta(X,Y,N) :- 
	mmatrix_seq(X,Y,_),
	N1 is N - 1,
	ejecuta(X,Y,N1).

speedups(N) :-
	main_seq(40,40,40,N).

main_seq(X,Y,Z,N) :-
	gen_list(X,Y,L1),
	gen_list(Z,Y,L2),
        statistics(walltime, [_,_]),
	ejecuta(L1,L2,N),
        statistics(walltime, [_,T2]),
	T is (T2 / N),
	display(time(T)),nl.

mmatrix_seq([],_,[]).
mmatrix_seq([R|RR],X,[Res|RRes]):-
        multiply_seq(X,R,Res),
        mmatrix_seq(RR,X,RRes).

multiply_seq([],_,[]).
multiply_seq([V0|Rest], V1, [Result|Others]):-
        vmul(V0,V1,Result),
        multiply_seq(Rest, V1, Others).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):-
        vmul(T1,T2, Newresult),
        Product is H1*H2,
        Result is Product+Newresult.

gen_list(0,_,[]) :- !.
gen_list(X,Y,[F|L1]) :-
	gen_list2(Y,F),
	X1 is X - 1,
	gen_list(X1,Y,L1).

gen_list2(0,[]) :- !.
gen_list2(X,[1|L1]) :-
	X1 is X - 1,
	gen_list2(X1,L1).
