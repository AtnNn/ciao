:- module(mandel,
	[
	    main_nondet/0,
	    main/0,
	    main/1
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	main_nondet.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq,
	between(1,8,N),
	main_nondet_par(N),
	fail.
main_nondet.

main_seq :-
	between(1,10,_),
        statistics(walltime, [T1,_]),
	demo_seq,
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_nondet_par(N) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	demo_par,
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
	format("-- mandelbrot, ~d agents, SpeedUp=~2f~n", [N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(_) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
        statistics(walltime, [T1,_]),
	demo_par,
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	format("-- mandelbrot=~f ms.~n", [Delta]),
	fail.
main(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data( 1, data(-1100,10,20,-300,25,15,5) ).
data( 2, data(-1100,10,30,-300,25,30,5) ).
data( 3, data(-500,10,20,-200,25,15,5) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

demo_seq :- demo_seq_(1).

demo_seq_(X):- data( X, Data ), go_demo_seq( Data ).

go_demo_seq( data(X, XI, XR, Y, YI, YR, Iterations) ) :-
	generate_seq( xrange(X,XI,XR), yrange(Y,YI,YR), Iterations, _Res ),
	fail.
go_demo_seq(_).

demo_par :- demo_par_(1).

demo_par_(X):- data( X, Data ), go_demo_par( Data ).

go_demo_par( data(X, XI, XR, Y, YI, YR, Iterations) ) :-
	generate_par( xrange(X,XI,XR), yrange(Y,YI,YR), Iterations, _Res ),
	fail.
go_demo_par(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_seq( xrange(XS,XI,XR), yrange(YS,YI,YR), Iterations,
          s(XPixelAdd,YPixelAdd,PChar) ) :-
        generate_val( XS, XI, XR, X, XPixelAdd ),
        generate_val( YS, YI, YR, Y, YPixelAdd ),
        fp_mult( X, X, XSQ ),
        fp_mult( Y, Y, YSQ ),
        ModSq is XSQ + YSQ,
        test_membership( c(X,Y), z0(X,Y), ModSq, Iterations, PChar ).

generate_par( xrange(XS,XI,XR), yrange(YS,YI,YR), Iterations,
          s(XPixelAdd,YPixelAdd,PChar) ) :-
        (
	    generate_val( XS, XI, XR, X, XPixelAdd ),
	    fp_mult( X, X, XSQ )
	)  &
        (
	    generate_val( YS, YI, YR, Y, YPixelAdd ),
	    fp_mult( Y, Y, YSQ )
	),
        ModSq is XSQ + YSQ,
	test_membership( c(X,Y), z0(X,Y), ModSq, Iterations, PChar ).

generate_val( Start, _Inc, Count, Start, Count ) :-
        Count > 0.
generate_val( Start, Inc, Count, Val, Address ) :-
        Count > 0,
        NStart is Start + Inc,
        NCount is Count - 1,
        generate_val( NStart, Inc, NCount, Val, Address ).

/* This predicate tests whether a point is inside the mandelbrot */
/* set or not.  The remainder returned is always a character     */
/* code to be used for display.                                  */

test_membership( c(R,I), z0(R0,I0), ModSq, Count, Char ) :-
	ModSq =< 4000,
	Count > 0  ,
        newr( NewR, R, I, R0 ),
        newi( NewI, R, I, I0 ),
        newmodsq( NewModSq, NewI, NewR ),
        NCount is Count - 1,
	test_membership( c(NewR,NewI), z0(R0,I0), NewModSq, NCount, Char ).
test_membership( _, _, ModSq, 0, h('.', ModSq) ) :-
	ModSq =< 4000.
test_membership( _, _, ModSq, Count, h('X', ModSq) ) :-
	Count > 0,
	ModSq > 4000.

newr( NewR, R, I, R0 ) :-
        fp_mult( R, R, RSQ ),
        fp_mult( I, I, ISQ ),
        NewR is RSQ - ISQ + R0.

newi( NewI, R, I, I0 ) :-
        fp_mult( R, I, RI ),
        NewI is (2*RI) + I0.

newmodsq( NewModSq, NewI, NewR ) :-
        fp_mult( NewR, NewR, NewRSQ ),
        fp_mult( NewI, NewI, NewISQ ),
        NewModSq is NewRSQ + NewISQ.


/* This routine implements fixed point multiplication with */
/* 3 decimal places.                                       */

fp_mult( A, B, C ) :-
        WholeNo is B//1000,
        Remainder is B-(WholeNo*1000),
        Tenths is Remainder//100,
        fp_mult_part2( A, Remainder, Tenths, PartResult ),
        C is (A*WholeNo) + ((A*Tenths)//10) + PartResult.

fp_mult_part2( A, Remainder, Tenths, PartResult ) :-
        Remainder2 is Remainder-(Tenths*100),
        Hundredths is Remainder2//10,
        Thousandths is Remainder2-(Hundredths*10),
        PartResult is ((A*Hundredths)//100) + ((A*Thousandths)//1000).
