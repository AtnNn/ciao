:- module(peano_list,
	    [
		peano_list/1,
		add_list/3,
		mul_list/3,
		vadd/3
	    ],
	    [
		library(codecoverage)
	    ]).

:- use_module(peano,
	    [
		peano_num/1,
		add/3,
		mul/3
	    ]).


peano_list([]).
peano_list([P|Ps]) :-
	peano_num(P),
	peano_list(Ps).

% add_list(Num,List,Res)

add_list(_,   [],     []).
add_list(Num, [P|Ps], [R|Rs]) :-
	add(Num, P, R),
	add_list(Num, Ps, Rs).

% mul_list(Num,List,Res)

mul_list(_,   [],     []).
mul_list(Num, [P|Ps], [R|Rs]) :-
	mul(Num, P, R),
	mul_list(Num, Ps, Rs).

% vprod(L1,L2,Res)

vadd([],     [],     []).
vadd([A|As], [B|Bs], [R|Rs]) :-
	add(A, B, R),
	vadd(As, Bs, Rs).
