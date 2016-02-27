:- module(boyer,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    wff/2,
	    tautology_seq/1,
	    tautology_det_gc/2,
	    tautology_nondet_gc/2
	],
	[fsyntax, andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_det :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(3),
	between(1,8,N),
	main_det_par(N),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(2),
	between(1,8,N),
	main_nondet_par(N),
	fail.
main_nondet.

main_seq(N) :-
	between(1,10,_),
	wff(N, Wff),
        statistics(walltime, [T1,_]),
	tautology_seq(Wff),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_det_par(N) :-
	ensure_agents(N),
	between(1,10,_),
	wff(3, Wff),
	pause(1),
        statistics(walltime, [T3,_]),
	tautology_det_gc(Wff,100),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_det_par(N) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- boyer, ~d agents, SpeedUp=~2f~n", [N,Sp]),
	fail.

main_nondet_par(N) :-
	ensure_agents(N),
	between(1,10,_),
	wff(2, Wff),
	pause(1),
        statistics(walltime, [T3,_]),
	tautology_nondet_gc(Wff,50),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(N) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- boyer, ~d agents, SpeedUp=~2f~n", [N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(_) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
	wff(3, Wff),
        statistics(walltime, [T1,_]),
	tautology_det_gc(Wff, 100),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- boyer, ~f ms.~n", [Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tautology_seq(Wff) :-
	s_rewrite(Wff,NewWff),
	tautology(NewWff,[],[]).

tautology_det_gc(Wff,Gran) :-
	p_rewrite_det_gc(Wff,Gran,NewWff),
	tautology(NewWff,[],[]).

tautology_nondet_gc(Wff,Gran) :-
	p_rewrite_ndet_gc(Wff,Gran,NewWff),
	tautology(NewWff,[],[]).

s_rewrite(A,A) :-
	A = (A1,_),
	atomic(A1),
	!.
s_rewrite(A,B) :-
	A = (A1,_),
	functor(A1,C,D),
	s_rewrite_args(D,C,A,E),
	's_rewrite/2/2/$disj/1'(B,_F,E),
	!.

s_rewrite_args(D,C,A,E) :-
	A = (A1,_Size),
	s_rewrite_args_(D,A1,L),
	E = ~join(D,C,L).

s_rewrite_args_(0,_,[]).
s_rewrite_args_(N,A,[Arg11|L]) :-
	arg(N,A,Arg),
	N1 is N - 1,
	s_rewrite(Arg,Arg1),
        s_rewrite_args_(N1,A,L),
	Arg11 = Arg1.

's_rewrite/2/2/$disj/1'(B,F,E) :-
	equal(E,F),
	s_rewrite(F,B).
's_rewrite/2/2/$disj/1'(B,_F,E) :-
	B=E.

p_rewrite_det_gc(A,_Gran,A) :-
	A = (A1,_),
	atomic(A1),
	!.
p_rewrite_det_gc(A,Gran,B) :-
	A = (A1,_),
	functor(A1,C,D),
	p_rewrite_args_det(D,Gran,C,A,E),
	'p_rewrite_det/2/2/$disj/1'(B,Gran,_F,E),
	!.

p_rewrite_args_det(D,Gran,C,A,E) :-
	A = (A1,Size),
	p_rewrite_args_det_(D,Gran,Size,A1,L),
	E = ~join(D,C,L).

p_rewrite_args_det_(0,_,_,_,[]).
p_rewrite_args_det_(N,Gran,Size,A,[Arg11|L]) :-
	arg(N,A,Arg),
	N1 is N - 1,
	(
	    Size > Gran ->
            p_rewrite_args_det_(N1,Gran,Size,A,L) '&!'
	    p_rewrite_det_gc(Arg,Gran,Arg1)
	;
            p_rewrite_args_det_(N1,Gran,Size,A,L),
	    p_rewrite_det_gc(Arg,Gran,Arg1)
	),
	Arg11 = Arg1.

'p_rewrite_det/2/2/$disj/1'(B,Gran,F,E) :-
	equal(E,F),
	p_rewrite_det_gc(F,Gran,B).
'p_rewrite_det/2/2/$disj/1'(B,_Gran,_F,E) :-
	B=E.

p_rewrite_ndet_gc(A,_Gran,A) :-
	A = (A1,_),
	atomic(A1),
	!.
p_rewrite_ndet_gc(A,Gran,B) :-
	A = (A1,_),
	functor(A1,C,D),
	p_rewrite_args_ndet(D,Gran,C,A,E),
	'p_rewrite_ndet/2/2/$disj/1'(B,Gran,_F,E),
	!.

p_rewrite_args_ndet(D,Gran,C,A,E) :-
	A = (A1,Size),
	p_rewrite_args_ndet_(D,Gran,Size,A1,L),
	E = ~join(D,C,L).

p_rewrite_args_ndet_(0,_,_,_,[]).
p_rewrite_args_ndet_(N,Gran,Size,A,[Arg11|L]) :-
	arg(N,A,Arg),
	N1 is N - 1,
	(
	    Size > Gran ->
            p_rewrite_args_ndet_(N1,Gran,Size,A,L) &
	    p_rewrite_ndet_gc(Arg,Gran,Arg1)
	;
            p_rewrite_args_ndet_(N1,Gran,Size,A,L),
	    p_rewrite_ndet_gc(Arg,Gran,Arg1)
	),
	Arg11 = Arg1.

'p_rewrite_ndet/2/2/$disj/1'(B,Gran,F,E) :-
	equal(E,F),
	p_rewrite_ndet_gc(F,Gran,B).
'p_rewrite_ndet/2/2/$disj/1'(B,_Gran,_F,E) :-
	B=E.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tautology(A,B,C) :-
	'tautology/3/1/$disj/1'(_D,_E,_F,C,B,A),
	!.

'tautology/3/1/$disj/1'(_D,_E,_F,_C,B,A) :-
	truep(A,B),
	!,
	true.
'tautology/3/1/$disj/1'(_D,_E,_F,C,_B,A) :-
	falsep(A,C),
	!,
	fail.
'tautology/3/1/$disj/1'(D,E,F,C,B,A) :-
	A = ~join3(if,F,E,D),
	!,
	'tautology/3/1/$disj/1/6/3/$disj/1'(D,C,E,B,F).

'tautology/3/1/$disj/1/6/3/$disj/1'(_D,C,E,B,F) :-
	truep(F,B),
	!,
	tautology(E,B,C).
'tautology/3/1/$disj/1/6/3/$disj/1'(D,C,_E,B,F) :-
	falsep(F,C),
	!,
	tautology(D,B,C).
'tautology/3/1/$disj/1/6/3/$disj/1'(D,C,E,B,F) :-
	tautology(E,[F|B],C),
	tautology(D,B,[F|C]).

truep((t,_),_A) :-
	!.

truep(A,B) :-
	boyer:member(A,B).

falsep((f,_),_A) :-
	!.

falsep(A,B) :-
	boyer:member(A,B).

member(A,[A|_B]) :-
	!.

member(A,[_B|C]) :-
	boyer:member(A,C).

equal((and(A,B),_),X) :-
	X = ~join3(if,A,(~join3(if,B,(t,1),(f,1))),(f,1)).

equal((append((append(A,B),_),C),_),X) :-
	X = ~join2(append,A,(~join2(append,B,C))).

equal((assignment(A,(append(B,C),_)),_),X) :-
	X = ~join3(if,(~join2(assignedp,A,B)),(~join2(assignedp,A,B)),(~join2(assignedp,A,C))).

equal((assume_false(A,B),_),X) :-
	X = ~join2(cons,(~join2(cons,A,(f,1))),B).

equal((assume_true(A,B),_),X) :-
	X = ~join2(cons,(~join2(cons,A,(t,1))),B).

equal((boolean(A),_),X) :-
	X = ~join2(or,(~join2(equal,A,(t,1))),(~join2(equal,A,(f,1)))).

equal((car((gopher(A),_)),_),X) :-
	X = ~join3(if,~join1(listp,A),~join1(car,(~join1(flatten,A))),(zero,1)).

equal((compile(A),_),X) :-
	X = ~join1(reverse,~join2(codegen,(~join1(optimize,A)),([],1))).

equal((count_list(A,(sort_lp(B,C),_)),_),X) :-
	X = ~join2(plus,(~join2(count_list,A,B)),(~join2(count_list,A,C))).

equal((countps_(A,B),_),X) :-
	X = ~join3(countps_loop,A,B,(zero,1)).

equal((difference(A,B),_),C) :-
	difference(A,B,C).

equal((divides(A,B),_),X) :-
	X = ~join1(zerop,(~join2(remainder,B,A))).

equal((dsort(A),_),X) :-
	X = ~join1(sort2,A).

equal((eqp(A,B),_),X) :-
	X = ~join2(equal,(~join1(fix,A)),(~join1(fix,B))).

equal((equal(A,B),_),C) :-
	eq(A,B,C).

equal((even1(A),_),X) :-
	X = ~join3(if,(~join1(zerop,A)),(t,1),(~join1(odd,(~join1(decr,A))))).

equal((exec((append(A,B),_),C,D),_),X) :-
	X = ~join3(exec,B,(~join3(exec,A,C,D)),D).

equal((exp(A,B),_),C) :-
	exp(A,B,C).

equal((fact_(A),_),X) :-
	X = ~join2(fact_loop,A,(1,1)).

equal((falsify(A),_),X) :-
	X = ~join2(falsify1,(~join1(normalize,A)),([],1)).

equal((fix(A),_),X) :-
	X = ~join3(if,(~join1(numberp,A)),A,(zero,1)).

equal((flatten((cdr((gopher(A),_)),_)),_),X) :-
	X = ~join3(if,(~join1(listp,A)),(~join1(cdr,(~join1(flatten,A)))),(~join2(cons,(zero,1),([],1)))).

equal((gcd(A,B),_),C) :-
	gcd(A,B,C).

equal((get(A,(set(B,C,D),_)),_),X) :-
	X = ~join3(if,(~join2(eqp,A,B)),C,(~join2(get,A,D))).

equal((greatereqp(A,B),_),X) :-
	X = ~join1(not,~join2(lessp,A,B)).

equal((greatereqpr(A,B),_),X) :-
	X = ~join1(not,~join2(lessp,A,B)).

equal((greaterp(A,B),_),X) :-
	X = ~join2(lessp,B,A).

equal((if((if(A,B,C),_),D,E),_),X) :-
	X = ~join3(if,A,(~join3(if,B,D,E)),(~join3(if,C,D,E))).

equal((iff(A,B),_),X) :-
	X = ~join2(and,(~join2(implies,A,B)),(~join2(implies,B,A))).

equal((implies(A,B),_),X) :-
	X = ~join3(if,A,(~join3(if,B,(t,1),(f,1))),(t,1)).

equal((last((append(A,B),_)),_),X) :-
	X = ~join3(if,(~join1(listp,B)),(~join1(last,B)),(~join3(if,(~join1(listp,A)),(~join1(cons,(~join1(car,(~join1(last,A)))))),B))).

equal((length(A),_),B) :-
	mylength(A,B).

equal((lesseqp(A,B),_),X) :-
	X = ~join1(not,(~join2(lessp,B,A))).

equal((lessp(A,B),_),C) :-
	lessp(A,B,C).

equal((listp((gopher(A),_)),_),X) :-
	X = ~join1(listp,A).

equal((mc_flatten(A,B),_),X) :-
	X = ~join2(append,~join1(flatten,A),B).

equal((meaning(A,B),_),C) :-
	meaning(A,B,C).

equal((member(A,B),_),C) :-
	mymember(A,B,C).

equal((not(A),_),X) :-
	X = ~join3(if,A,(f,1),(t,1)).

equal((nth(A,B),_),C) :-
	nth(A,B,C).

equal((numberp(greatest_factor(A,B)),_),X) :-
	X = ~join1(not,(~join2(and,(~join2(or,(~join1(zerop,B)),(~join2(equal,B,1)))),(~join1(not,(~join1(numberp,A))))))).

equal((or(A,B),_),X) :-
	X = ~join3(if,A,(t,1),(~join3(if,B,(t,1),(f,1)))).

equal((plus(A,B),_),C) :-
	plus(A,B,C).

equal((power_eval(A,B),_),C) :-
	power_eval(A,B,C).

equal((prime(A),_),X) :-
	X = ~join2(and,(~join1(not,(~join1(zerop,A)))),(~join2(and,(~join1(not,(~join2(equal,A,(~join1(add1,(zero,1))))))),(~join2(prime1,A,(~join1(decr,A))))))).

equal((prime_list(append(A,B)),_),X) :-
	X = ~join2(and,(~join1(prime_list,A)),(~join1(prime_list,B))).

equal((quotient(A,B),_),C) :-
	quotient(A,B,C).

equal((remainder(A,B),_),C) :-
	remainder(A,B,C).

equal((reverse_(A),_),X) :-
	X = ~join2(reverse_loop,A,([],1)).

equal((reverse(append(A,B)),_),X) :-
	X = ~join2(append,(~join1(reverse,B)),(~join1(reverse,A))).

equal((reverse_loop(A,B),_),C) :-
	reverse_loop(A,B,C).

equal((samefringe(A,B),_),X) :-
	X = ~join2(equal,(~join1(flatten,A)),(~join1(flatten,B))).

equal((sigma(zero,A),_),X) :-
	X = ~join2(quotient,(~join2(times,A,(~join1(add1,A)))),(2,1)).

equal((sort2(delete(A,B)),_),X) :-
	X = ~join2(delete,A,(~join1(sort2,B))).

equal((tautology_checker(A),_),X) :-
	X = ~join2(tautologyp,(~join1(normalize,A)),([],1)).

equal((times(A,B),_),C) :-
	times(A,B,C).

equal((times_list(append(A,B)),_),X) :-
	X = ~join2(times,(~join1(times_list,A)),(~join1(times_list,B))).

equal((value(normalize(A),B),_),X) :-
	X = ~join2(value,A,B).

equal((zerop(A),_),X) :-
	X = ~join2(or,(~join2(equal,A,(zero,1))),(~join1(not,~join1(numberp,A)))).

difference(A,A,(zero,1)) :-
	!.

difference((plus(A,B),_),A,X) :-
	X = ~join1(fix,B),
	!.

difference((plus(A,B),_),B,X) :-
	X = ~join1(fix,A),
	!.

difference((plus(A,B),_),plus(A,C),X) :-
	X = ~join2(difference,B,C),
	!.

difference((plus(A,plus(B,C)),_),B,X) :-
	X = ~join2(plus,A,C),
	!.

difference((add1(plus(A,B)),_),B,X) :-
	X = ~join1(add1,A),
	!.

difference((add1(add1(A)),_),(2,_),X) :-
	X = ~join1(fix,A).

eq((plus(A,B),_),(zero,_),X) :-
	X = and(zerop(A),zerop(B)),
	!.

eq((plus(A,B),_),(plus(A,C),_),X) :-
	X = ~join2(equal,(~join1(fix,B)),(~join1(fix,C))),
	!.

eq((zero,_),(difference(A,B),_),X) :-
	X = ~join1(not,(~join2(lessp,B,A))),
	!.

eq(A,(difference(A,B),_),X) :-
	X = ~join2(and,(~join1(numberp,A)),(~join2(or,(~join2(equal,A,(zero,1))),(~join1(zerop,B))))),
	!.

eq((times(A,B),_),(zero,_),X) :-
	X = ~join2(or,(~join1(zerop,A)),(~join1(zerop,B))),
	!.

eq((append(A,B),_),(append(A,C),_),X) :-
	X = ~join2(equal,B,C),
	!.

eq((flatten(A),_),(cons(B,([],_)),_),X) :-
	X = ~join2(and,(~join1(nlistp,A)),(~join2(equal,A,B))),
	!.

eq((greatest_factor(A,B),_),(zero,_),X) :-
	X = ~join2(and,(~join2(or,(~join1(zerop,B)),(~join2(equal,B,1)))),(~join2(equal,A,(zero,1)))),
	!.

eq((greatest_factor(A,_B),_),(1,_),X) :-
	X = ~join2(equal,A,(1,1)),
	!.

eq(A,(times(B,A),_),X) :-
	X = ~join2(and,(~join1(numberp,A)),(~join2(or,(~join2(equal,A,(zero,1))),(~join2(equal,B,(1,1)))))),
	!.

eq(A,(times(A,B),_),X) :-
	X = ~join2(or,(~join2(equal,A,(zero,1))),(~join2(and,(~join1(numberp,A)),(~join2(equal,B,(1,1)))))),
	!.

eq((times(A,B),_),(1,_),X) :-
	X = ~join2(and,(~join1(not,(~join2(equal,A,(zero,1))))),(~join2(and,(~join1(not,(~join2(equal,B,(zero,1))))),(~join2(and,(~join1(numberp,A)),(~join2(and,(~join1(numberp,B)),(~join2(and,(~join2(equal,(~join1(decr,A)),(zero,1))),(~join2(equal,(~join1(decr,B)),(zero,1)))))))))))),
	!.

eq((difference(A,B),_),(difference(C,B),_),X) :-
	X = ~join3(if,(~join2(lessp,A,B)),(~join1(not,(~join2(lessp,B,C)))),(~join3(if,(~join2(lessp,C,B)),(~join1(not,(~join2(lessp,B,A)))),(~join2(equal,(~join1(fix,A)),(~join1(fix,C))))))),
	!.

eq((lessp(A,B),_),C,X) :-
	X = ~join3(if,(~join2(lessp,A,B)),(~join2(equal,(t,1),C)),(~join2(equal,(f,1),C))).

exp(A,(plus(B,C),_),X) :-
	X = ~join2(time,(~join2(exp,A,B)),(~join2(exp,A,C))),
	!.

exp(A,(times(B,C),_),X) :-
	X = ~join2(exp,(~join2(exp,A,B)),C).

gcd(A,B,X) :-
	X = ~join2(gcd,B,A),
	!.

gcd((times(A,B),_),(times(C,B),_),X) :-
	X = ~join2(times,B,(~join2(gcd,A,C))).

mylength((reverse(A),_),X) :-
	X = ~join1(length,A).

mylength((cons(_A,(cons(_B,(cons(_C,(cons(_D,(cons(_E,(cons(_F,G),_)),_)),_)),_)),_)),_),X) :-
	X = ~join2(plus,(6,1),(~join1(length,G))).

lessp((remainder(_A,B),_),B,X) :-
	X = ~join1(not,(~join1(zerop,B))),
	!.

lessp((quotient(A,B),_),A,X) :-
	X = ~join2(and,(~join1(not,(~join1(zerop,A)))),(~join2(or,(~join1(zerop,B)),(~join1(not,(~join2(equal,B,(1,1)))))))),
	!.

lessp((remainder(A,B),_),A,X) :-
	X = ~join2(and,(~join1(not,(~join1(zerop,B)))),(~join2(and,(~join1(not,(~join1(zerop,A)))),(~join1(not,(~join2(lessp,A,B))))))),
	!.

lessp((plus(A,B),_),(plus(A,C),_),X) :-
	X = ~join2(lessp,B,C),
	!.

lessp((times(A,B),_),(times(C,B),_),X) :-
	X = ~join2(and,(~join1(not,(~join1(zerop,B)))),(~join2(lessp,A,C))),
	!.

lessp(A,(plus(B,A),_),X) :-
	X = ~join1(not,(~join1(zerop,B))),
	!.

lessp((length((delete(A,B),_)),_),(length(B),_),X) :-
	X = ~join2(member,A,B).

meaning((plus_tree((append(A,B),_)),_),C,X) :-
	X = ~join2(plus,(~join2(meaning,(~join1(plus_tree,A)),C)),(~join2(meaning,(~join1(plus_tree,B)),C))),
	!.

meaning((plus_tree((plus_fringe(A),_)),_),B,X) :-
	X = ~join1(fix,(~join2(meaning,A,B))),
	!.

meaning((plus_tree((delete(A,B),_)),_),C,X) :-
	X = ~join3(if,(~join2(member,A,B)),(~join2(difference,(~join2(meaning,(~join1(plus_tree,B)),C)),(~join2(meaning,A,C)))),(~join2(meaning,(~join1(plus_tree,B)),C))).

mymember(A,(append(B,C),_),X) :-
	X = ~join2(or,(~join2(member,A,B)),(~join2(member,A,C))),
	!.

mymember(A,(reverse(B),_),X) :-
	X = ~join2(member,A,B),
	!.

mymember(A,(intersect(B,C),_),X) :-
	X = ~join2(and,(~join2(member,A,B)),(~join2(member,A,C))).

nth((zero,1),_A,(zero,1)).

nth(([],1),A,X) :-
	X = ~join3(if,(~join1(zerop,A)),([],1),(zero,1)).

nth((append(A,B),_),C,X) :-
	X = ~join2(append,(~join2(nth,A,C)),(~join2(nth,B,(~join2(difference,C,(~join1(length,A))))))).

plus((plus(A,B),_),C,X) :-
	X = ~join2(plus,A,(~join2(plus,B,C))),
	!.

plus((remainder(A,B),_),(times(B,(quotient(A,B),_)),_),X) :-
	X = ~join1(fix,A),
	!.

plus(A,(add1(B),_),X) :-
	X = ~join3(if,(~join1(numberp,B)),(~join1(add1,(~join2(plus,A,B)))),(~join1(add1,A))).

power_eval((big_plus1(A,B,C),_),C,X) :-
	X = ~join2(plus,(~join2(power_eval,A,C)),B),
	!.

power_eval((power_rep(A,B),_),B,X) :-
	X = ~join1(fix,A),
	!.

power_eval((big_plus(A,B,C,D),_),D,X) :-
	X = ~join2(plus,C,(~join2(plus,(~join2(power_eval,A,D)),(~join2(power_eval,B,D))))),
	!.

power_eval((big_plus((power_rep(A,B),_),(power_rep(C,B),_),(zero,_),B),_),B,X) :-
	X = ~join2(plus,A,C).

quotient((plus(A,(plus(A,B),_)),_),(2,_),X) :-
	X = ~join2(plus,A,(~join2(quotient,B,2))).

quotient((times(A,B),_),A,X) :-
	X = ~join3(if,(~join1(zerop,A)),(zero,1),(~join1(fix,B))).

remainder(_A,(1,1),(zero,1)) :-
	!.

remainder(A,A,(zero,1)) :-
	!.

remainder((times(_A,B),_),B,(zero,1)) :-
	!.

remainder((times(A,_B),_),A,(zero,1)).

reverse_loop(A,B,X) :-
	X = ~join2(append,(~join1(reverse,A)),B),
	!.

reverse_loop(A,([],1),X) :-
	X = ~join1(reverse,A).

times(A,(plus(B,C),_),X) :-
	X = ~join2(plus,(~join2(times,A,B)),(~join2(times,A,C))),
	!.

times((times(A,B),_),C,X) :-
	X = ~join2(times,A,(~join2(times,B,C))),
	!.

times(A,(difference(B,C),_),X) :-
	X = ~join2(difference,(~join2(times,B,A)),(~join2(times,C,A))),
	!.

times(A,(add1(B),_),X) :-
	X = ~join3(if,(~join1(numberp,B)),(~join2(plus,A,(~join2(times,A,B)))),(~join1(fix,A))).

%% These are the old modes, some of them have bugs, so don't trust them:
%%
%% :- mode['tautology/3/1/$disj/1'(-,-,-,+,+,+),
%% 	'tautology/3/1/$disj/1/6/3/$disj/1'(+,+,+,+,+),
%% 	times(?,?,-),
%% 	tautology(+,+,+),
%% 	rewrite_args(+,+,+),
%% 	'rewrite/2/2/$disj/1'(?,-,+),
%% 	reverse_loop(?,?,-),
%% 	remainder(?,?,-),
%% 	quotient(?,?,-),
%% 	power_eval(?,?,-),
%% 	plus(?,?,-),
%% 	nth(?,?,-),
%% 	mymember(?,?,-),
%% 	meaning(?,?,-),
%% 	lessp(?,?,-),
%% 	gcd(?,?,-),
%% 	exp(?,?,-),
%% 	eq(?,?,-),
%% 	difference(?,?,-),
%% 	truep(+,+),
%% 	rewrite(+,?),
%% 	mylength(?,-),
%% 	member(+,+),
%% 	falsep(+,+),
%% 	equal(+,-),
%% 	wff(-),
%% 	time(?),
%% 	tautology(+),
%% 	go(?)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% size(X,1) :-
% 	atomic(X),
% 	!.
% size(X,T) :-
% 	X =.. [_|A],
% 	size_(A,0,T2),
% 	T is T2 + 1.
% size_([],X,X).
% size_([A|L],X,T) :-
% 	size(A,S1),
% 	S2 is S1 + X,
% 	size_(L,S2,T).

size_wff((_,T),T).

% compute_sizes(X,Y,Z,A,B,C,D) :-
% 	size(~wff(0),X),
% 	size(~wff(1),Y),
% 	size(~wff(2),Z),
% 	size(~wff(3),A),
% 	size(~wff(4),B),
% 	size(~wff(5),C),
% 	size(~wff(6),D).

join(N, F, L, R) :-
	( N == 1 -> L = [A], R = ~join1(F,A)
	; N == 2 -> L = [B,A], R = ~join2(F,A,B)
	; N == 3 -> L = [C,B,A], R = ~join3(F,A,B,C)
	).
join1(F, A, (Exp,Size)) :-
	Exp =.. [F,A],
	A = (_EA,TA),
	(ground(TA) -> Size is TA + 1 ; true).

join2(F, A, B, (Exp,Size)) :-
	Exp =.. [F,A,B],
	A = (_EA,TA),
	B = (_EB,TB),
	(ground(TA),ground(TB) -> Size is TA + TB + 1 ; true).

join3(F, A, B, C, (Exp,Size)) :-
	Exp =.. [F,A,B,C],
	A = (_EA,TA),
	B = (_EB,TB),
	C = (_EC,TC),
	(ground(TA),ground(TB),ground(TC) -> Size is TA + TB + TC + 1 ; true).

% Size = 3
wff(0,X) :-
	X = ~join2(implies,(a,1),(a,1)).

% Size = 7
wff(1,X) :-
	X = ~join2(implies,(~join2(and,(~join2(implies,(a,1),(b,1))),(a,1))),(b,1)).

% Size = 55
wff(2,X) :-
	A = ~join1(f,(~join2(plus,(~join2(plus,(a,1),(b,1))),(~join2(plus,(c,1),(zero,1)))))),
	B = ~join1(f,(~join2(times,(~join2(times,(a,1),(b,1))),(~join2(times,(c,1),(d,1)))))),
	C = ~join2(lessp,(~join2(remainder,(a,1),(b,2))),(~join2(member,(a,1),(~join1(length,(b,1)))))),
	X = ~join2(implies,(~join2(and,(~join2(implies,A,B)),(~join2(implies,B,C)))),(~join2(implies,A,C))).

% Size = 85
wff(3,X) :-
	A = ~join1(f,(~join2(plus,(~join2(plus,(a,1),(b,1))),(~join2(plus,(c,1),(zero,1)))))),
	B = ~join1(f,(~join2(times,(~join2(times,(a,1),(b,1))),(~join2(plus,(c,1),(d,1)))))),
	C = ~join1(f,(~join1(reverse,(~join2(append,(~join2(append,(a,1),(b,1))),([],1)))))),
	D = ~join2(equal,(~join2(plus,(a,1),(b,1))),(~join2(difference,(x,1),(y,1)))),
	E = ~join2(lessp,(~join2(remainder,(a,1),(b,1))),(~join2(member,(a,1),(~join1(length,(b,1)))))),
	X = ~join2(implies,(~join2(and,(~join2(implies,A,B)),(~join2(and,(~join2(implies,B,C)),(~join2(and,(~join2(implies,C,D)),(~join2(implies,D,E)))))))),(~join2(implies,A,E))).

% Size = 111
wff(4,X) :-
	wff(2,A),
	wff(2,B),
	X = ~join2(implies,A,B).

% Size = 71
wff(5,X) :-
	wff(2,A),
	wff(1,B),
	wff(1,C),
	X = ~join2(implies,(~join2(implies,A,B)),C).

% Size = 101
wff(6,X) :-
	wff(3,A),
	wff(1,B),
	wff(1,C),
	X = ~join2(implies,(~join2(implies,A,B)),C).

