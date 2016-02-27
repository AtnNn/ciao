:- module(deriv,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    prepare_gc/3,
	    d_seq/3,
	    d_det_gc/4,
	    d_ndet_gc/4
	],
	[fsyntax, andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
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
	main_seq(100),
	between(1,8,N),
	main_det_par(100,N),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(100),
	between(1,8,N),
	main_nondet_par(100,N),
	fail.
main_nondet.

main_seq(X) :-
	between(1,10,_),
	prepare_gc(X,Exp,V),
        statistics(walltime, [T1,_]),
	d_seq(Exp,V,_),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_det_par(X,N) :-
	ensure_agents(N),
	between(1,10,_),
	prepare_gc(X,Exp,V),
	pause(1),
        statistics(walltime, [T3,_]),
	d_det_gc(Exp,V,100,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_det_par(X,N) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- deriv(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(X,N) :-
	ensure_agents(N),
	between(1,10,_),
	prepare_gc(X,Exp,V),
	pause(1),
        statistics(walltime, [T3,_]),
	d_ndet_gc(Exp,V,100,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(X,N) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- deriv(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
	prepare_gc(X,Exp,V),
	statistics(walltime, [T1,_]),
	d_det_gc(Exp,V,100,_),
	statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- deriv(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d(U+V,X,DU+DV)	         :- !,d(V,X,DV),d(U,X,DU).
d(U-V,X,DU-DV)	         :- !,d(V,X,DV),d(U,X,DU).
d(U*V,X,DU*V+U*DV)       :- !,d(V,X,DV),d(U,X,DU).
d(U/V,X,(DU*V-U*DV)/V^2) :- !,d(V,X,DV),d(U,X,DU).
d(U^N,X,DU*N*U^N1)       :- !,integer(N),N1 is N-1,d(U,X,DU).
d(-U,X,-DU)              :- !,d(U,X,DU).
d(exp(U),X,exp(U)*DU)    :- !,d(U,X,DU).
d(log(U),X,DU/U)         :- !,d(U,X,DU).
d(X,X,1)                 :- !.
d(_C,_X,0).

d_par_det(U+V,X,DU+DV)           :- !,d_par_det(V,X,DV) '&!' d_par_det(U,X,DU).
d_par_det(U-V,X,DU-DV)           :- !,d_par_det(V,X,DV) '&!' d_par_det(U,X,DU).
d_par_det(U*V,X,DU*V+U*DV)       :- !,d_par_det(V,X,DV) '&!' d_par_det(U,X,DU).
d_par_det(U/V,X,(DU*V-U*DV)/V^2) :- !,d_par_det(V,X,DV) '&!' d_par_det(U,X,DU).
d_par_det(U^N,X,DU*N*U^N1)       :- !,integer(N),N1 is N-1,d_par_det(U,X,DU).
d_par_det(-U,X,-DU)              :- !,d_par_det(U,X,DU).
d_par_det(exp(U),X,exp(U)*DU)    :- !,d_par_det(U,X,DU).
d_par_det(log(U),X,DU/U)         :- !,d_par_det(U,X,DU).
d_par_det(X,X,1)                 :- !.
d_par_det(_C,_X,0).

d_par_ndet(U+V,X,DU+DV)           :- !,d_par_ndet(V,X,DV) & d_par_ndet(U,X,DU).
d_par_ndet(U-V,X,DU-DV)           :- !,d_par_ndet(V,X,DV) & d_par_ndet(U,X,DU).
d_par_ndet(U*V,X,DU*V+U*DV)       :- !,d_par_ndet(V,X,DV) & d_par_ndet(U,X,DU).
d_par_ndet(U/V,X,(DU*V-U*DV)/V^2) :- !,d_par_ndet(V,X,DV) & d_par_ndet(U,X,DU).
d_par_ndet(U^N,X,DU*N*U^N1)       :- !,integer(N),N1 is N-1,d_par_ndet(U,X,DU).
d_par_ndet(-U,X,-DU)              :- !,d_par_ndet(U,X,DU).
d_par_ndet(exp(U),X,exp(U)*DU)    :- !,d_par_ndet(U,X,DU).
d_par_ndet(log(U),X,DU/U)         :- !,d_par_ndet(U,X,DU).
d_par_ndet(X,X,1)                 :- !.
d_par_ndet(_C,_X,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Benchmarks obtained with threshold of size 100 (the expression
% generated has a size of 230 and the benchmark runs with a expression
% that is the sum of N of these expressions).

size((_,T),T).

d_seq((U+V,_),X,DU+DV)           :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U-V,_),X,DU-DV)           :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U*V,_),X,DU*V+U*DV)       :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U/V,_),X,(DU*V-U*DV)/V^2) :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U^N,_),X,DU*N*U^N1)       :- !,N=(N2,T),integer(N2),N3 is N2-1,N1=(N3,T),d(U,X,DU).
d_seq((-U,_),X,-DU)              :- !,d_seq(U,X,DU).
d_seq((exp(U),_),X,exp(U)*DU)    :- !,d_seq(U,X,DU).
d_seq((log(U),_),X,DU/U)         :- !,d_seq(U,X,DU).
d_seq((X,_),X,1)                 :- !.
d_seq(_,_,0).

d_gc(E,X,Gran,R) :- ~size(E) > Gran -> d_det_gc(E,X,Gran,R)  ; d_seq(E,X,R).
d_ngc(E,X,Gran,R):- ~size(E) > Gran -> d_ndet_gc(E,X,Gran,R) ; d_seq(E,X,R).

d_det_gc((U+V,_),X,Gran,DU+DV)           :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_det_gc((U-V,_),X,Gran,DU-DV)           :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_det_gc((U*V,_),X,Gran,DU*V+U*DV)       :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_det_gc((U/V,_),X,Gran,(DU*V-U*DV)/V^2) :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_det_gc((U^N,_),X,Gran,DU*N*U^N1)       :- !,N=(N2,T),integer(N2),N3 is N2-1,N1=(N3,T),d_gc(U,X,Gran,DU).
d_det_gc((-U,_),X,Gran,-DU)              :- !,d_gc(U,X,Gran,DU).
d_det_gc((exp(U),_),X,Gran,exp(U)*DU)    :- !,d_gc(U,X,Gran,DU).
d_det_gc((log(U),_),X,Gran,DU/U)         :- !,d_gc(U,X,Gran,DU).
d_det_gc((X,_),X,_,1)                    :- !.
d_det_gc(_,_,_,0).

d_ndet_gc((U+V,_),X,Gran,DU+DV)           :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_ndet_gc((U-V,_),X,Gran,DU-DV)           :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_ndet_gc((U*V,_),X,Gran,DU*V+U*DV)       :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_ndet_gc((U/V,_),X,Gran,(DU*V-U*DV)/V^2) :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_ndet_gc((U^N,_),X,Gran,DU*N*U^N1)       :- !,N=(N2,T),integer(N2),N3 is N2-1,N1=(N3,T),d_ngc(U,X,Gran,DU).
d_ndet_gc((-U,_),X,Gran,-DU)              :- !,d_ngc(U,X,Gran,DU).
d_ndet_gc((exp(U),_),X,Gran,exp(U)*DU)    :- !,d_ngc(U,X,Gran,DU).
d_ndet_gc((log(U),_),X,Gran,DU/U)         :- !,d_ngc(U,X,Gran,DU).
d_ndet_gc((X,_),X,_,1)                    :- !.
d_ndet_gc(_,_,_,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


prepare(N,LExp,V) :-
	mexpr(N,LExp),
	V = x,
	!.

mexpr(0,x).
mexpr(X, Exp + Rest) :- 
	X>0,
	expression(Exp), 
	Y is X-1,
	mexpr(Y,Rest).

expression( Exp + Exp - Exp * Exp / Exp * Exp / Exp ) :- value(Exp).

value(((3*x + (4*exp(x^3)*log(x^2)) -2) / ( -(3*x) + 5/(exp(x^4)+2)))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


prepare_gc(N,LExp,V) :-
	mexpr_gc(N,LExp),
	V = x,
	!.

mexpr_gc(X,Y) :-
	mexpr_gc_(X,(0,1),Y).
mexpr_gc_(0,X,X).
mexpr_gc_(X,E,FinalExp) :- 
	X>0,
	!,
	expression_gc(Exp), 
	X1 is X-1,
	mexpr_gc_(X1,(~join2(E, +, Exp)),FinalExp).

% Size of E is 230.
expression_gc(E) :-
	value_gc(Exp),
	E = (~join2((~join2(Exp,+,Exp)),-,(~join2(Exp, *, (~join2(Exp,/,(~join2(Exp, *, (~join2(Exp, *, Exp)))))))))).

join1(Op, A, (Exp, T)):-
        Exp =.. [Op, A],
        A = (_EA, TA),
        T is TA + 1.

join2(A, Op, B, (Exp, T)):-
        Exp =.. [Op, A, B],
        A = (_EA, TA),
        B = (_EB, TB),
        T is TA + TB + 1.

% Size of E is 32.
value_gc(E) :-
	N = (~join2((~join2((3,1), *, (x,1))), +, (~join2((~join2((~join2((4,1), *, (~join1(exp, (~join2((x,1), ^, (3,1))))))), *, (~join1(log, (~join2((x,1), ^, (2,1))))))), -, (2,1))))),
	M = (~join2((~join1(-, (~join2((3,1), *, (x,1))))), +, (~join2((5,1), /, (~join2((~join1(exp, (~join1(exp, (~join2((x,1), ^, (4,1))))))), +, (2,1))))))),
	E = (~join2(N, /, M)).

