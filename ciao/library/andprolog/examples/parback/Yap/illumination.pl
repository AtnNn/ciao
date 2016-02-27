:- module(illumination,
        [
	    speedups/0
	]).


:- table get_colum1/4.


speedups :-
	R = [[0,1,1,0,0,0,1,0],[1,1,0,0,1,0,1,0],[0,0,1,1,0,0,0,1],[0,0,0,0,1,0,0,0],
	[0,1,1,0,0,1,0,0],[0,0,0,1,1,0,1,0],[0,0,1,1,0,0,0,1],[0,0,1,0,0,1,0,0]],
	main_seq(R).

just_first(C) :-
	call(C), !.

main_seq(R) :-
	    abolish_all_tables,
        statistics(walltime, [_,_]),
	just_first(find_ind(R,3,L)),
        statistics(walltime, [_,T]),
	display(time_first(T)),nl,
	fail.

main_seq(R) :-
	abolish_all_tables,
        statistics(walltime, [_,_]),
	(
	    find_ind(R,3,L), fail
	;
	    true
	),
        statistics(walltime, [_,T]),
	display(time_all(T)),nl.

find_ind(Board,D,R) :-
	throw_colum_ndet(1,Board,R,D).

throw_colum_ndet(N,[C],[R],_) :- 
	!,get_colum1(N,C,1,R).

throw_colum_ndet(N,[C|RC],[R1,R2|RR],D) :- 
	N1 is N + 1,
	throw_colum_ndet(N1,RC,[R2|RR],D),
	get_colum1(N,C,1,R1),
	check_value(R1,R2,D).

get_colum1(_,C,1,R1) :-
 	get_colum(C,1,R1).

get_colum([],_,_) :- !, fail.
get_colum([C|_],Ind,R) :- 
	pause,
	C == 1,
	Ind = R.
get_colum([_|RC],Ind,R) :- 
	Ind1 is Ind + 1,
	get_colum(RC,Ind1,R).

check_value(R1,R2,D) :-
	R1 > R2,
	R1 - R2 >= D.

check_value(R1,R2,D) :-
	R2 - R1 >= D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_board(Type,P,C,L,R) :- 
	iterate_col(C,Type,P,L,R).


iterate_col(0,_,_,_,[]) :- !.
iterate_col(C,Type,P,L,[R|Rs]) :-
	C1 is C - 1,
	get_line(L,Type,P,R),
	iterate_col(C1,Type,P,L,Rs).

get_line(0,_,_,[]) :- !.
get_line(L,Type,P,[R|Rs]) :-
	L1 is L - 1,
	get_number(Type,P,R),
	get_line(L1,Type,P,Rs).

get_number(bin,P,R) :-
	random(1,10,N),
	N =< P,
	!, R = 1.

get_number(bin,_P,0).

get_number(dec,_,R) :-
	random(1,10,R).

pause :- fib(15,_).

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
	fib(N2, F2),
        F is F1 + F2.

