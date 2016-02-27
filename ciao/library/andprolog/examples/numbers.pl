:- module(numbers,
	[
	    main/0,
	    main/2,
	    numbers_seq/3,
	    numbers_par/5
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(system)).
:- use_module(library(apll)).
:- use_module(library(arithpreds), [floor/2]).
:- use_module(library(prolog_sys)).
:- use_module(library(lists), [length/2]).
:- use_module(library(write)).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	Number = 31,
	List = [4,4,4,4],
	length(List,Size),
	Gran = Size,
	main_seq(Number,List),
	between(1,8,N),
	main_nondet_par(N,Size,Gran,Number,List),
	fail.
main.

main_seq(N,L) :-
	between(1,10,_),
        statistics(walltime, [T1,_]),
	numbers_seq(N,_,L),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_,_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_nondet_par(NAg,Size,Gran,N,L) :-
	ensure_agents(NAg),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	numbers_par(N,Size,Gran,_,L),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(NAg,_,_,N,L) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),LPar),
	average(LPar,P),
	SpUp is 100*(Seq/P),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- numbers(~f,~w), ~d agents, SpeedUp=~2f TPar=~f~n",
               [N,L,NAg,Sp,P]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(N,L) :-
	set_prolog_flag(gc, off),
	ensure_agents(1),
	length(L,Size),
	Gran = Size,
        statistics(walltime, [T1,_]),
% 	numbers_seq(N,E,L),
	numbers_par(N,Size,Gran,_,L),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	format("-- numbers(~f,~w)=~f ms.~n", [N,L,Delta]),
	fail.
main(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

numbers_seq(N,E,L) :-
	lex_seq(N,E,L),
	fail.
numbers_seq(_,_,_).

numbers_par(N,S,G,E,L) :-
	lex_par(N,S,G,E,L),
	fail.
numbers_par(_,_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- data found/2.
% :- data impossible/3.


 %lex(N, E, L): look for an expression E, using numbers in L which has
 %value N.  For example,
 % ?- lex(100, E, [1,2,3,4,5,6,7]).
 %
 % E = 1+(2+(3+sqrt(4)*(5+6*7))) ? 

lex_seq(N, E, L):-
	%sort(L, L1),
	find_xp_seq(N, E, L).

lex_par(N, S, G, E, L):-
	%sort(L, L1),
	find_xp_par(N, S, G, E, L).


 % find(+X, ?E, +L): the expression E uses all the numbers in the list
 % L and its numerical value  is X. By now, X is restricted to be integer.


find_xp_seq(X, E, L):-
	\+ impossible_case(L, E, X),  % Argument order for indexation
        find_xp_seq_maybe(X, E, L).

find_xp_par(X, S, G, E, L):-
	\+ impossible_case(L, E, X),  % Argument order for indexation
        find_xp_par_maybe(X, S, G, E, L).


find_xp_seq_maybe(X, X, [X]):- !.
find_xp_seq_maybe(X, sqrt(T), [T]):- X * X =:= T, !.
% find_xp_seq_maybe(X, E, L):-
% 	already_found(X, E, L), !.
find_xp_seq_maybe(X, E, L):-
	directed_search_seq(X, E, L), !.
% 	found(X, E, L).
find_xp_seq_maybe(X, E, L):-
	search_seq(X, E, L), !.
% 	found(X, E, L).
% find_xp_seq_maybe(X, _, L):-                    % Here: we must be sure!
% 	impossible(X, L),
% 	fail.


find_xp_par_maybe(X, _, _, X, [X]):- !.
find_xp_par_maybe(X, _, _, sqrt(T), [T]):- X * X =:= T, !.
% find_xp_par_maybe(X, _, _, E, L):-
% 	already_found(X, E, L), !.
find_xp_par_maybe(X, S, G, E, L):-
	directed_search_par(X, S, G, E, L), !.
% 	found(X, E, L).
find_xp_par_maybe(X, S, G, E, L):-
	search_par(X, S, G, E, L), !.
% 	found(X, E, L).
% find_xp_par_maybe(X, _, _, _, L):-              % Here: we must be sure!
% 	impossible(X, L),
% 	fail.


 % A brief list of impossible cases to reduce database overhead.

impossible_case([], _, _).
%impossible_case([Y], _, X):- X =\= Y.  % These two are to
impossible_case([Y, Z], _, X):-        % reduce database overhead with
	\+ operate(Y, Z, X).           % small cases
% impossible_case(L, _, X):-
% 	already_impossible(X, L).
 
operate(A, B, C):- C is A + B.
operate(A, B, C):- C is A - B.
operate(A, B, C):- C is B - A.
operate(A, B, C):- C is A * B.
operate(A, B, C):- B =\= 0, C is A / B.
operate(A, B, C):- A =\= 0, C is B / A.


 % directed_search(+X, ?E, +L): the expression E uses al the numbers in
 % the list L and its numerical value is X. Integer descompositions are
 % sought for. This is quite tricky, because the final
 % search knows about it to reduce the search space.

directed_search_seq(X, E, L):-
	asim_subset(L, _, L1, _, L2, _),
	directed_com_seq(X, E, L1, L2).
% directed_search_seq(X, E, L):-
% 	nep_subset(L, La, Lb),
% 	already_found(Xa, A, La),
% 	Xa >= X,
% 	directed_nocom_seq(X, Xa, E, A, Lb).


directed_search_par(X, S, G, E, L):-
	asim_subset(L, _, L1, S1, L2, S2),
	directed_com_par(X, S, G, E, S1, L1, S2, L2).
% directed_search_par(X, S, G, E, L):-
% 	nep_subset(L, La, Lb),
% 	already_found(Xa, A, La),
% 	Xa >= X,
% 	directed_nocom_par(X, S, G, Xa, E, A, Lb).


 % Search for commutative operations.

directed_com_seq(X, A + B, La, Lb):-
	fac_add(X, Xa, Xb),     % X = Xa + Xb, both natural numbers.
	find_xp_seq(Xa, A, La),
	find_xp_seq(Xb, B, Lb).
directed_com_seq(X, A * B, La, Lb):-
	fac_mul(X, Xa, Xb),  % X = Xa * Xb
	find_xp_seq(Xa, A, La),
	find_xp_seq(Xb, B, Lb).


directed_com_par(X, S, G, A + B, Sa, La, Sb, Lb):-
	fac_add(X, Xa, Xb),     % X = Xa + Xb, both natural numbers.
	(
	    S < G ->
	    find_xp_seq(Xa, A, La),
	    find_xp_seq(Xb, B, Lb)
	;
	    find_xp_par(Xa, Sa, G, A, La) &
	    find_xp_par(Xb, Sb, G, B, Lb)
	).
directed_com_par(X, S, G, A * B, Sa, La, Sb, Lb):-
	fac_mul(X, Xa, Xb),  % X = Xa * Xb
	(
	    S < G ->
	    find_xp_seq(Xa, A, La),
	    find_xp_seq(Xb, B, Lb)
	;
	    find_xp_par(Xa, Sa, G, A, La) &
	    find_xp_par(Xb, Sb, G, B, Lb)
	).


 % search for non-commutative operations.  "Xa" already known.

directed_nocom_seq(X, Xa, A - B, A, Lb):-     % X = Xa - Xb
	Xb is Xa - X,
	find_xp_seq(Xb, B, Lb).
directed_nocom_seq(X, Xa, B - A, A, Lb):-     % X = Xb - Xa
	Xb is Xa + X,
	find_xp_seq(Xb, B, Lb).
directed_nocom_seq(X, Xa, A / B, A, Lb):-    % X = Xa / Xb
        X =\= 0,
	Xb is Xa // X,
        Xa =:= X * Xb,                    % Only integer allowed to find_xp
	find_xp_seq(Xb, B, Lb).
directed_nocom_seq(X, Xa, B / A, A, Lb):-    % X = Xb / Xa
	Xb is Xa * X,
	find_xp_seq(Xb, B, Lb).


directed_nocom_par(X, G, Xa, A - B, A, Sb, Lb):-     % X = Xa - Xb
	Xb is Xa - X,
	find_xp_par(Xb, Sb, G, B, Lb).
directed_nocom_par(X, G, Xa, B - A, A, Sb, Lb):-     % X = Xb - Xa
	Xb is Xa + X,
	find_xp_par(Xb, Sb, G, B, Lb).
directed_nocom_par(X, G, Xa, A / B, A, Sb, Lb):-    % X = Xa / Xb
        X =\= 0,
	Xb is Xa // X,
        Xa =:= X * Xb,                    % Only integer allowed to find_xp
	find_xp_par(Xb, Sb, G, B, Lb).
directed_nocom_par(X, G, Xa, B / A, A, Sb, Lb):-    % X = Xb / Xa
	Xb is Xa * X,
	find_xp_par(Xb, Sb, G, B, Lb).


 % search(?X, ?Y, +Z): the number X can be expressed by the expression Y
 % using the numbers in the list Z. As a heuristic rule, addition and
 % multiplication are tried before.

search_seq(X, X, [X]):- !.                  % Green.
search_seq(X, E, L):-
	asim_subset(L, _, La, _, Lb, _),
	search_seq(Xa, A, La),
	search_seq(Xb, B, Lb),
	oper_comm(Xa, Xb, A, B, X, E).
search_seq(X, E, L):-
	asim_subset(L, _, La, _, Lb, _),
	search_seq(Xa, A, La),
	search_seq(Xb, B, Lb),
	oper_nocomm(Xa, Xb, A, B, X, E).

search_par(X, _, _, X, [X]):- !.                  % Green.
search_par(X, _, G, E, L):-
	asim_subset(L, _, La, Sa, Lb, Sb),
	search_par(Xa, Sa, G, A, La),
	search_par(Xb, Sb, G, B, Lb),
	oper_comm(Xa, Xb, A, B, X, E).
search_par(X, _, _, E, L):-
	asim_subset(L, _, La, Sa, Lb, Sb),
	search_par(Xa, Sa, G, A, La),
	search_par(Xb, Sb, G, B, Lb),
	oper_nocomm(Xa, Xb, A, B, X, E).

oper_comm(Xa, Xb, A, B, X, A + B):- perform(Xa + Xb, X).
oper_comm(Xa, Xb, A, B, X, A * B):- perform(Xa * Xb, X).

oper_nocomm(Xa, Xb, A, B, X, A - B):- perform(Xa - Xb, X).
oper_nocomm(Xa, Xb, A, B, X, B - A):- perform(Xb - Xa, X).
oper_nocomm(Xa, Xb, A, B, X, A / B):- perform(Xa / Xb, X).
oper_nocomm(Xa, Xb, A, B, X, B / A):- perform(Xb / Xa, X).


 % perform(+Op, ?Res): Op is a rational or an operation between two
 % rationals, and Res is the result (rational) of that operation.

perform(Op, Res):-
	rewrite(Op, Op1),
	reduce(Op1, Res).


 % rewrite(?X, ?Y): X is a rational or an expression involving TWO
 % rationals and Y is the same expression rewrote so that the main
 % functor (if any) is '/'/2.

rewrite(A / B + C / D, (A * D + B * C) / (B * D)).
rewrite(A / B + C, (A + C * B) / B ):- atomic(C).
rewrite(C + A / B, (A + C * B) / B):- atomic(C).
rewrite(A + B, A + B):- atomic(A), atomic(B).
rewrite(A / B - C / D,  (A * D - B * C) / (B * D)).
rewrite(A / B - C, (A - B * C) / B):- atomic(C).
rewrite(C - A / B, (C * B - A) / B):- atomic(C).
rewrite(A - B, A - B):- atomic(A), atomic(B).
rewrite((A / B) * (C / D), (A * C) / (B * D)).
rewrite(A * (B / C), (A * B) / C):- atomic(A).
rewrite((A / B) * C, (A * C) / B):- atomic(C).
rewrite(A * B, A * B):- atomic(A), atomic(B).
rewrite((A / B) / (C / D), (A * D) / (B * C)).
rewrite(A / (B / C), (A * C) / B):- atomic(A).
rewrite((A / B) / C, A / (B * C)):- atomic(C).
rewrite(A / B, A / B):- atomic(A), atomic(B).
rewrite(A, A):- atomic(A).


 %

reduce(_ / Y, _):- 
        0 =:= Y, !, fail.
reduce(X / Y, Z):- !,
	(0 =:= X mod Y ->
	     Z is X // Y;
            Z = X / Y
	).
reduce(X, Y):- Y is X.


 % fac_add(X, Y, Z): X = Y + Z, X, Y and Z natural numbers.

fac_add(X, Y, Z):-
	X2 is X // 2,
	X1 is X - X2,
	fac_add2(X2, X1, Y, Z).
fac_add2(X, Y, X1, Y1):- swap(X, Y, X1, Y1).
fac_add2(X, Y, X1, Y1):-
	X > 0,
	Xa is X - 1,
	Ya is Y + 1,
	fac_add2(Xa, Ya, X1, Y1).


 % fac_mul(X, Y, Z): X = Y * Z, X, Y and Z natural numbers.

fac_mul(X, Y, Z):-
	sqrt(X, X2),
	fac_mul2(X, X2, Y1, Z1),
	swap(Y1, Z1, Y, Z).
fac_mul2(X, Xd, Xd, D):-
	Xd =\= 0,
	0 =:= X mod Xd,
	D is X // Xd.
fac_mul2(X, Xd, Y, Z):-
	Xd > 0,
	Xd1 is Xd - 1,
	fac_mul2(X, Xd1, Y, Z).


 % Horrible integer square root algorithm.

sqrt(1, 1):- !.
sqrt(X, Y):-
	sqrt2(X, X, 0, Y).
sqrt2(X, Max, Min, Y):-
	Med is (Max + Min) // 2,
	sqrt3(X, Max, Med, Min, Y).
sqrt3(_, __, Med,  Min, Min):-
	Med - Min < 1, !.
sqrt3(X, Max, Med, Min, Y):-
	(
	        Med * Med =< X ->
		sqrt2(X, Max, Med, Y)
	;
	        sqrt2(X, Med, Min, Y)
	).

 % Only different numbers are swapped.

swap(X, Y, X, Y).
swap(X, Y, Y, X):- X =\= Y.


 % subset(X, Y, Z): Y and Z are subsets of X. Relative order is preserved.

subset([], [], []).
subset([X|Xs], Ys, [X|Zs]):- subset(Xs, Ys, Zs).
subset([X|Xs], [X|Ys], Zs):- subset(Xs, Ys, Zs).


 % nep_subset(X, Y, Z): Y and Z are non-empty subsets of X

nep_subset(X, Y, Z):-
	Y = [_|_],
	Z = [_|_],
	subset(X, Y, Z).


 % asim_subset(X, Y, Z): Y and Z are non-empty subsets of X and a
 % solution given for Y on backtracking will never be returned for Z

asim_subset([X|Xs], Sx, [X|Ys], Sy, Zs, Sz):-
	Zs = [_|_],
	subset(Xs, Ys, Zs),
	length([X|Xs],Sx),
	length([X|Ys],Sy),
	length(Zs,Sz).


 % select(X, Y, Z): the element X is caught from the list Y to give
 % the list Z.

select(X, [X|Y], Y).
select(X, [_|Y], Z):-
	select(X, Y, Z).

 
 %

 %% member(X, [X|_]).
 %% member(X, [_|L]):- member(X, L).


 % Definition of a natural number.

natural(0).
natural(X):-
	natural(Y),
	X is Y + 1.


 % Only one solution.

% one_express(X, Y, Z):- express(X, Y, Z), !.


 %

% time(T) :- statistics(runtime,[_,T]).


 % Database

 % found(+X, +E, +L): asserts that the expression E, using numbers in L,
 % has value X. The database has an entry for each different list, and
 % the expressions are organised as a binary tree indexed by the
 % number to be obtained.


% found(X, E, List):-
% 	(
% 	        current_fact(found(List, Tree), Ref) ->
% 		    erase(Ref)
% 	;
% 	        Tree = nil
% 	),
% 	insert(Tree, X, E, NewTree),
% 	asserta_fact(found(List, NewTree)).


 % already_found(?X, ?E, ?L): the tuple (X, E, L) has already been
 % found.

% already_found(X, E, List):-
% 	List = [_, _, _|_],
% 	current_fact(found(List,Tree)),
% 	visit(Tree, X, E).


 % impossible(+X, +L): states that the number X can't de obtained from
 % numbers in L. A database of triples (A, B, L) is maintained; each
 % of them mean that the numbers form A to B can't be obtained from
 % the numbers in L.

% impossible(X, L):-
% 	current_fact(impossible(L, A, B), Ref),
% 	impossible2(X, A, B, L, Ref), !.
% impossible(X, L):-
%         asserta_fact(impossible(L, X, X)).

impossible2(X, A, B, _, _):-
	X >= A, X =< B, !.
impossible2(X, A, _B, _L, Ref):-
	X =:= A - 1, !,
	erase(Ref).
% 	asserta_fact(impossible(L, X, B)).
impossible2(X, _A, B, _L, Ref):-
	X =:= B + 1, !,
	erase(Ref).
% 	asserta_fact(impossible(L, A, X)).


 % already_impossible(+X, +L): the number X can't be derived from the
 % ones in  L. This is done by maintaining tuples (A, B, L), which mean "the
 % numbers from A to B can't be obtained from the ones in L".

% already_impossible(X, L):-
% 	current_fact(impossible(L, A, B)),
% 	A =< X, B >= X, !.


 % Clean the whole database.

% clean:-
%         retractall_fact(found(_,_)),
%         retractall_fact(impossible(_,_,_)).


%  % Print the database.

% all:- all_pos, fail.
% all:- all_impos, fail.
% all.


%  % Print all the posibilities found so far

% all_pos:-
% 	nl, write('Possible: '), nl,
% 	already_found(X, E, L),
% 	tab(4), write([X, E, L]), nl,
% 	fail.
% all_pos.


%  % Id. with the not possible ones.

% all_impos:-
% 	nl, write('Impossible: '), nl,
% 	current_fact(impossible(L, A, B)),
% 	tab(4), write([A, B, L]), nl,
% 	fail.
% all_impos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 % An implementation of balanced trees
 % structure: avl(Key, Value, LeftSon, RightSon, Depth, Balance).
 % Balance is LeftSon depth minus RightSon depth. Since Depth is
 % max(depth(LeftSon),depth(RightSon)) + 1, this information is enough
 % to know the depth of each son.

insert(nil, K, V, avl(K, V, nil, nil, 1, 0)).
insert(avl(K, V, L, R, D, B), K, V, avl(K, V, L, R, D, B)).
insert(avl(K1, V1, Lson, Rson, _, _), K, V, NewTree):-
	K < K1,
	insert(Lson, K, V, NewLeft),
	compose_balance(NewLeft, Rson, Nb),
	balance(Nb, K1, V1, NewLeft, Rson, NewTree).
insert(avl(K1, V1, Lson, Rson, _, _), K, V, NewTree):-
	K > K1,
	insert(Rson, K, V, NewRight),
	compose_balance(Lson, NewRight, Nb),
	balance(Nb, K1, V1, Lson, NewRight, NewTree).


balance(B, K1, V1, L, R, avl(K1, V1, L, R, D, Nb)):-
	B < 2, B > -2, !,
	compose_balance(L, R, Nb),
	compose_depth(L, R, D).

 % Balance left branch.

balance(2, Kb, Vb, avl(Ka, Va, S1, S2, _, Bl), S3, T):-
	(
	        Bl > 0 ->
		    T = avl(Ka, Va, S1, Rt, Dt, 0),
		        Rt = avl(Kb, Vb, S2, S3, Db, Bb),
			    compose_balance(S2, S3, Bb)
	;
	        T = avl(Kc, Vc, Lt, Rt, Dt, 0),
		    Lt = avl(Ka, Va, S1, S4, Dlt, Blt),
		        Rt = avl(Kb, Vb, S5, S3, Db, Brt),
			    S2 = avl(Kc, Vc, S4, S5, _, _),
			        compose_balance(S5, S3, Brt),
				    compose(S1, S4, Dlt, Blt)
	),
	find_depth(S3, D3),
	Dt is D3 + 2,
	Db is D3 + 1.

 % Balance right branch

balance(-2, Kb, Vb, S3, avl(Ka, Va, S2, S1, _, Bl), T):-
	(
	        Bl < 0 ->
		    T = avl(Ka, Va, Lt, S1, Dt, 0),
		        Lt = avl(Kb, Vb, S3, S2, Db, Bb),
			    compose_balance(S3, S2, Bb)
	;
	        T = avl(Kc, Vc, Lt, Rt, Dt, 0),
		    Rt = avl(Ka, Va, S4, S1, Dlt, Blt),
		        Lt = avl(Kb, Vb, S3, S5, Db, Brt),
			    S2 = avl(Kc, Vc, S5, S4, _, _),
			        compose_balance(S5, S3, Brt),
				    compose(S1, S4, Dlt, Blt)
	),
	find_depth(S3, D3),
	Dt is D3 + 2,
	Db is D3 + 1.



compose_balance(A, B, Nb):-
	find_depth(A, Da),
	find_depth(B, Db),
	Nb is Da - Db.

compose_depth(A, B, Nd):-
	find_depth(A, Da),
	find_depth(B, Db),
	(Da > Db -> Nd is Da; Nd is Db).

compose(L, R, D, B):-
	find_depth(L, Dl),
	find_depth(R, Dr),
	B is Dl - Dr,
	(Dl > Dr -> D is Dl; D is Dr).

find_depth(avl(_,_,_,_,Dl,_), Dl).
find_depth(nil, 0).

visit(avl(K, V, _, _, _, _), K, V).
visit(avl(_, _, L, _, _, _), K, V):- visit(L, K, V).
visit(avl(_, _, _, R, _, _), K, V):- visit(R, K, V).


