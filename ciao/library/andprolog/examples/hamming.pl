:- module(hamming,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    ham_seq/1,
	    ham_det/1,
	    ham_nondet/1
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(system)).
:- use_module(library(sort)).
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
	main_seq(1000),
	between(1,8,N),
	main_det_par(N,1000),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(1000),
	between(1,8,N),
	main_nondet_par(N,1000),
	fail.
main_nondet.

main_seq(X) :-
	between(1,10,_),
        statistics(walltime, [T1,_]),
	ham_seq(X),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_det_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	ham_det(X),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_det_par(N,X) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- hamming(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	ham_nondet(X),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(N,X) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- hamming(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
        statistics(walltime, [T1,_]),
	ham_det(X),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- hamming(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ham_seq(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming_seq(Number,Twos,Threes,Fives,S1,S2,S3), ! .

hamming_seq(0,_1,_2,_3,_,_,_).
hamming_seq(N,Twos,Threes,Fives,_S2s,_S3s,_S5s) :-
        N>0,
	firstq(Twos,FTwo),
	firstq(Threes,FThree),
	firstq(Fives,FFive),
	select_smaller(FTwo,FThree,FFive,Smaller),
	try_dequeue(Twos,Smaller,NewTwos),
	try_dequeue(Threes,Smaller,NewThrees),
	try_dequeue(Fives,Smaller,NewFives),
	New2 is Smaller*2,
	New3 is Smaller*3,
	New5 is Smaller*5,
	enqueue(New2,NewTwos,OtherTwos,S2snew),
	enqueue(New3,NewThrees,OtherThrees,S3snew),
	enqueue(New5,NewFives,OtherFives,S5snew),
	N1 is N-1,
        hamming_seq(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew).

ham_det(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming_det(Number,Twos,Threes,Fives,S1,S2,S3), ! .

hamming_det(0,_1,_2,_3,_,_,_).
hamming_det(N,Twos,Threes,Fives,S2s,_S3s,_S5s) :-
        N>0,
	(
	    (S2s > 20) ->
	     firstq(Twos,FTwo) '&!'
	     firstq(Threes,FThree) '&!'
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Twos,Smaller,NewTwos) '&!>' H6,
	     try_dequeue(Threes,Smaller,NewThrees) '&!>' H7,
	     try_dequeue(Fives,Smaller,NewFives) '&!>' H8,
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     H6 '<&!',
	     enqueue(New2,NewTwos,OtherTwos,S2snew) '&!>' H12,
	     H7 '<&!',
	     enqueue(New3,NewThrees,OtherThrees,S3snew) '&!>' H13,
	     H8 '<&!',
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1,
	     H12 '<&!',
	     H13 '<&!'
	;
	     firstq(Twos,FTwo),
	     firstq(Threes,FThree),
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Threes,Smaller,NewThrees),
	     try_dequeue(Fives,Smaller,NewFives),
	     try_dequeue(Twos,Smaller,NewTwos),
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     enqueue(New2,NewTwos,OtherTwos,S2snew),
	     enqueue(New3,NewThrees,OtherThrees,S3snew),
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1
	),
        hamming_det(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew).

ham_nondet(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming_nondet(Number,Twos,Threes,Fives,S1,S2,S3).

hamming_nondet(0,_1,_2,_3,_,_,_).
hamming_nondet(N,Twos,Threes,Fives,S2s,_S3s,_S5s) :-
        N>0,
	(
	    (S2s > 20) ->
	     firstq(Twos,FTwo) &
	     firstq(Threes,FThree) &
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Twos,Smaller,NewTwos) &> H6,
	     try_dequeue(Threes,Smaller,NewThrees) &> H7,
	     try_dequeue(Fives,Smaller,NewFives) &> H8,
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     H6 <& ,
	     enqueue(New2,NewTwos,OtherTwos,S2snew) &> H12,
	     H7 <& ,
	     enqueue(New3,NewThrees,OtherThrees,S3snew) &> H13,
	     H8 <& ,
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1,
	     H12 <& ,
	     H13 <&
	;
	     firstq(Twos,FTwo),
	     firstq(Threes,FThree),
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Threes,Smaller,NewThrees),
	     try_dequeue(Fives,Smaller,NewFives),
	     try_dequeue(Twos,Smaller,NewTwos),
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     enqueue(New2,NewTwos,OtherTwos,S2snew),
	     enqueue(New3,NewThrees,OtherThrees,S3snew),
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1
	),
        hamming_nondet(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew).

emptyq([]).

firstq(Q,X) :-
        append(_1,[X],Q) .

enqueue(X,Q,[X|Q],S) :-
	length([X|Q],S).

dequeue(Q,W,NQ) :-
        append(NQ,[W],Q) .

try_dequeue(Q,What,NewQ) :-
        firstq(Q,What),
        !,
        dequeue(Q,What,NewQ) .
try_dequeue(Q,_What,NewQ) :-
        Q=NewQ .

select_smaller(A,B,C,Min) :-
        min(A,B,Ma),
        min(Ma,C,Min) .

min(A,B,C) :-
        A<B,
        !,
        C=A .
min(_A,B,C) :-
        C=B .


