:- module(table_performance, [main/0], [parback]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(system)).
:- use_module(library(apll_parback), [release_all_for_unwinding/0]).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(write)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- use_module(aiakl).
:- use_module(ann).
:- use_module(boyer).
:- use_module(deriv).
:- use_module(fft).
:- use_module(fibo).
:- use_module(hamming).
:- use_module(hanoi).
:- use_module(hanoi_dl).
:- use_module(mandel).
:- use_module(mmat).
:- use_module(pal).
:- use_module(progeom).
:- use_module(qsort).
:- use_module(qsort_dl).
:- use_module(queens).
:- use_module(tak).

:- data t_aux/2.
:- data t_seq/2.
:- data t_det/3.
:- data t_nondet/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	set_prolog_flag(gc, off),
	retractall_fact(t_aux(_,_)),
	retractall_fact(t_seq(_,_)),
	retractall_fact(t_det(_,_,_)),
	retractall_fact(t_nondet(_,_,_)),
	display('----------------'), nl,
	display('   SEQUENTIAL   '), nl,
	display('----------------'), nl,
	main_comp_seq,
	between(1,8,N),
	display('----------------'), nl,
	display('   AGENTS:  '), display(N), nl,
	display('----------------'), nl,
	release_all_for_unwinding,
	main_comp_det(N),
	release_all_for_unwinding,
	main_comp_nondet(N),
	fail.
main :-
	write_table,
	display('Table with performance results written.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_comp_seq :-
	between(1,10,_),
	aiakl:prepare(20,L1,L2),
	measure_time(aiakl,init_vars_seq(L1,L2,_,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(aiakl,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(aiakl,Seq)),
	format("-- aiakl(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	ann:prepare(20,ManyClauses),
	measure_time(ann,analyze_all_seq(ManyClauses,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(ann,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(ann,Seq)),
	format("-- ann(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	wff(2, Wff),
	measure_time(boyer,tautology_seq(Wff)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(boyer,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(boyer,Seq)),
	format("-- boyer(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	prepare_gc(100,Exp,V),
	measure_time(deriv,d_seq(Exp,V,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(deriv,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(deriv,Seq)),
	format("-- deriv(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(fft,fft_seq),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(fft,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(fft,Seq)),
	format("-- fft(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(fibo,fib_seq(25,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(fibo,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(fibo,Seq)),
	format("-- fibo(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(hamming,ham_seq(1000)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(hamming,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(hamming,Seq)),
	format("-- ham(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(hanoi,hanoi_seq(14,_,_,_,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(hanoi,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(hanoi,Seq)),
	format("-- hanoi(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(hanoi_dl,hanoidl_seq(14,_,_,_,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(hanoi_dl,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(hanoi_dl,Seq)),
	format("-- hanoi_dl(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	mmat:gen_list(30,30,L1),
	mmat:gen_list(30,30,L2),
	measure_time(mmat,mmatrix_seq(L1,L2,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(mmat,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(mmat,Seq)),
	format("-- mmat(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	pal:gen_list(15,L),
	measure_time(pal,palindrome_seq(L,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(pal,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(pal,Seq)),
	format("-- pal(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(progeom,pds_seq(5,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(progeom,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(progeom,Seq)),
	format("-- progeom(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	qsort:gen_list(3000,L),
	measure_time(qsort,qsort_seq(L,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(qsort,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(qsort,Seq)),
	format("-- qsort(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	qsort_dl:gen_list(3000,L),
	measure_time(qsort_dl,qsortdl_seq(L,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(qsort_dl,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(qsort_dl,Seq)),
	format("-- qsort_dl(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(queens,q_seq(10,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(queens,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(queens,Seq)),
	format("-- queens(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :-
	between(1,10,_),
	measure_time(tak,tak_seq(14,10,3,_)),
	fail.
main_comp_seq :-
	findall(S,retract_fact(t_aux(tak,S)),LSeq),
	average(LSeq,Seq),
	assertz_fact(t_seq(tak,Seq)),
	format("-- tak(seq)=~f ms.~n", [Seq]),
	fail.
main_comp_seq :- nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_comp_det(N) :-
	ensure_agents(N),
	between(1,10,_),
	aiakl:prepare(20,L1,L2),
	measure_time(aiakl,init_vars_det(L1,L2,_,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(aiakl,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(aiakl,Par,N)),
	current_fact(t_seq(aiakl,Seq)),
	Sp is Seq/Par,
	format("-- aiakl(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	ann:prepare(20,ManyClauses),
	measure_time(ann,analyze_all_par_det(ManyClauses,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(ann,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(ann,Par,N)),
	current_fact(t_seq(ann,Seq)),
	Sp is Seq/Par,
	format("-- ann(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	wff(2, Wff),
	measure_time(boyer,tautology_det_gc(Wff,100)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(boyer,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(boyer,Par,N)),
	current_fact(t_seq(boyer,Seq)),
	Sp is Seq/Par,
	format("-- boyer(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	prepare_gc(100,Exp,V),
	measure_time(deriv,d_det_gc(Exp,V,100,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(deriv,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(deriv,Par,N)),
	current_fact(t_seq(deriv,Seq)),
	Sp is Seq/Par,
	format("-- deriv(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	measure_time(fft,fft_det_gc(20)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(fft,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(fft,Par,N)),
	current_fact(t_seq(fft,Seq)),
	Sp is Seq/Par,
	format("-- fft(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	measure_time(fibo,fib_det_gc(25,15,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(fibo,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(fibo,Par,N)),
	current_fact(t_seq(fibo,Seq)),
	Sp is Seq/Par,
	format("-- fibo(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	measure_time(hamming,ham_det(1000)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(hamming,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(hamming,Par,N)),
	current_fact(t_seq(hamming,Seq)),
	Sp is Seq/Par,
	format("-- ham(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	measure_time(hanoi,hanoi_det_gc(14,_,_,_,7,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(hanoi,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(hanoi,Par,N)),
	current_fact(t_seq(hanoi,Seq)),
	Sp is Seq/Par,
	format("-- hanoi(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	measure_time(hanoi_dl,hanoidl_det_gc(14,_,_,_,7,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(hanoi_dl,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(hanoi_dl,Par,N)),
	current_fact(t_seq(hanoi_dl,Seq)),
	Sp is Seq/Par,
	format("-- hanoi_dl(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	mmat:gen_list(30,30,L1),
	mmat:gen_list(30,30,L2),
	measure_time(mmat,mmatrix_det(L1,L2,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(mmat,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(mmat,Par,N)),
	current_fact(t_seq(mmat,Seq)),
	Sp is Seq/Par,
	format("-- mmat(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	pal:gen_list(15,L),
	measure_time(pal,palindrome_det_gc(L,_,7)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(pal,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(pal,Par,N)),
	current_fact(t_seq(pal,Seq)),
	Sp is Seq/Par,
	format("-- pal(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(N) :-
	Par = '-',
	assertz_fact(t_det(progeom,Par,N)),
	display('-- '), display(progeom(det,N)), display('=-'), nl,
	fail.
main_comp_det(_) :-
	between(1,10,_),
	qsort:gen_list(3000,L),
	measure_time(qsort,qsort_det_gc(L,3000,500,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(qsort,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(qsort,Par,N)),
	current_fact(t_seq(qsort,Seq)),
	Sp is Seq/Par,
	format("-- qsort(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :-
	between(1,10,_),
	qsort_dl:gen_list(3000,L),
	measure_time(qsort_dl,qsortdl_det_gc(L,3000,500,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(qsort_dl,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(qsort_dl,Par,N)),
	current_fact(t_seq(qsort_dl,Seq)),
	Sp is Seq/Par,
	format("-- qsort_dl(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(N) :-
	Par = '-',
	assertz_fact(t_det(queens,Par,N)),
	display('-- '), display(queens(det,N)), display('=-'), nl,
	fail.
main_comp_det(_) :-
	between(1,10,_),
	measure_time(tak,tak_det_gc(14,10,3,_)),
	fail.
main_comp_det(N) :-
	findall(S,retract_fact(t_aux(tak,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_det(tak,Par,N)),
	current_fact(t_seq(tak,Seq)),
	Sp is Seq/Par,
	format("-- tak(det,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_det(_) :- nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_comp_nondet(N) :-
	ensure_agents(N),
	between(1,10,_),
	aiakl:prepare(20,L1,L2),
	measure_time(aiakl,init_vars_nondet(L1,L2,_,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(aiakl,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(aiakl,Par,N)),
	current_fact(t_seq(aiakl,Seq)),
	Sp is Seq/Par,
	format("-- aiakl(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	ann:prepare(20,ManyClauses),
	measure_time(ann,analyze_all_par_nondet(ManyClauses,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(ann,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(ann,Par,N)),
	current_fact(t_seq(ann,Seq)),
	Sp is Seq/Par,
	format("-- ann(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	wff(2, Wff),
	measure_time(boyer,tautology_nondet_gc(Wff,100)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(boyer,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(boyer,Par,N)),
	current_fact(t_seq(boyer,Seq)),
	Sp is Seq/Par,
	format("-- boyer(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	prepare_gc(100,Exp,V),
	measure_time(deriv,d_ndet_gc(Exp,V,100,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(deriv,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(deriv,Par,N)),
	current_fact(t_seq(deriv,Seq)),
	Sp is Seq/Par,
	format("-- deriv(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(fft,fft_nondet_gc(20)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(fft,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(fft,Par,N)),
	current_fact(t_seq(fft,Seq)),
	Sp is Seq/Par,
	format("-- fft(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(fibo,fib_nondet_gc(25,15,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(fibo,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(fibo,Par,N)),
	current_fact(t_seq(fibo,Seq)),
	Sp is Seq/Par,
	format("-- fibo(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(hamming,ham_nondet(1000)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(hamming,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(hamming,Par,N)),
	current_fact(t_seq(hamming,Seq)),
	Sp is Seq/Par,
	format("-- ham(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(hanoi,hanoi_nondet_gc(14,_,_,_,7,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(hanoi,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(hanoi,Par,N)),
	current_fact(t_seq(hanoi,Seq)),
	Sp is Seq/Par,
	format("-- hanoi(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(hanoi_dl,hanoidl_nondet_gc(14,_,_,_,7,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(hanoi_dl,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(hanoi_dl,Par,N)),
	current_fact(t_seq(hanoi_dl,Seq)),
	Sp is Seq/Par,
	format("-- hanoi_dl(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	mmat:gen_list(30,30,L1),
	mmat:gen_list(30,30,L2),
	measure_time(mmat,mmatrix_nondet(L1,L2,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(mmat,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(mmat,Par,N)),
	current_fact(t_seq(mmat,Seq)),
	Sp is Seq/Par,
	format("-- mmat(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	pal:gen_list(15,L),
	measure_time(pal,palindrome_nondet_gc(L,_,7)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(pal,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(pal,Par,N)),
	current_fact(t_seq(pal,Seq)),
	Sp is Seq/Par,
	format("-- pal(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(progeom,pds_par(5,2,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(progeom,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(progeom,Par,N)),
	current_fact(t_seq(progeom,Seq)),
	Sp is Seq/Par,
	format("-- progeom(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	qsort:gen_list(3000,L),
	measure_time(qsort,qsort_nondet_gc(L,3000,500,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(qsort,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(qsort,Par,N)),
	current_fact(t_seq(qsort,Seq)),
	Sp is Seq/Par,
	format("-- qsort(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	qsort_dl:gen_list(3000,L),
	measure_time(qsort_dl,qsortdl_nondet_gc(L,3000,500,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(qsort_dl,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(qsort_dl,Par,N)),
	current_fact(t_seq(qsort_dl,Seq)),
	Sp is Seq/Par,
	format("-- qsort_dl(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(queens,q_par(10,10,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(queens,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(queens,Par,N)),
	current_fact(t_seq(queens,Seq)),
	Sp is Seq/Par,
	format("-- queens(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :-
	between(1,10,_),
	measure_time(tak,tak_nondet_gc(14,10,3,_)),
	fail.
main_comp_nondet(N) :-
	findall(S,retract_fact(t_aux(tak,S)),LPar),
	average(LPar,Par),
	assertz_fact(t_nondet(tak,Par,N)),
	current_fact(t_seq(tak,Seq)),
	Sp is Seq/Par,
	format("-- tak(nondet,~d)=~f ms., SpeedUp=~f.~n", [N,Par,Sp]),
	fail.
main_comp_nondet(_) :- nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

measure_time(Id,Goal) :-
        statistics(walltime, [T1,_]),
	call(Goal),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	assertz_fact(t_aux(Id,Delta)).

write_table :-
	open('table_performance.tex',write,File),
	write(File,'\\begin{table}[t]'), nl(File),
	write(File,'  \\centering'), nl(File),
	write(File,'  \\begin{tabular}[b]{|c|c||c|c|c|c|c|c|c|c|c|}'), nl(File),
	write(File,'    \\whline{\\hseplinew}'), nl(File),
	write(File,'    \\multirow{2}{*}{\\textbf{Benchmark}} &'), nl(File),
	write(File,'    \\multirow{2}{*}{\\textbf{Op.}} &'), nl(File),
	write(File,'    \\multicolumn{9}{c|}{\\textbf{Number of agents}}'), nl(File),
	write(File,'    \\\\ \\cline{3-11}'), nl(File),
	write(File,'    & & \\ Seq. \\ & \\ \\ \\ 1 \\ \\ \\ & \\ \\ \\ 2 \\ \\ \\ & \\ \\ \\ 3 \\ \\ \\ &'), nl(File),
	write(File,'   \\ \\ \\ 4 \\ \\ \\ & \\ \\ \\ 5 \\ \\ \\ & \\ \\ \\ 6 \\ \\ \\ & \\ \\ \\ 7 \\ \\ \\ &'), nl(File),
	write(File,'   \\ \\ \\ 8 \\ \\ \\ \\\\\\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{AIAKL}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,aiakl), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,aiakl), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Ann}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,ann), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,ann), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Boyer}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,boyer), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,boyer), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Deriv}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,deriv), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,deriv), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{FFT}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,fft), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,fft), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Fibonacci}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,fibo), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,fibo), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Hamming}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,hamming), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,hamming), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Hanoi}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,hanoi), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,hanoi), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{HanoiDL}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,hanoi_dl), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,hanoi_dl), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{MMatrix}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,mmat), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,mmat), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Palindrome}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,pal), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,pal), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Progeom}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 & - & - & - & - & - & - & - & -'), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,progeom), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{QuickSort}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,qsort), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,qsort), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{QuickSortDL}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 '), write_det_speedups(File,qsort_dl), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,qsort_dl), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Queens}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00 & - & - & - & - & - & - & - & -'), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00 '), write_nondet_speedups(File,queens), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'\\multirow{2}{*}{Takeuchi}'), nl(File),
	write(File,'     & \\ \\textsf{\\&!} & 1.00'), write_det_speedups(File,tak), nl(File),
	write(File,'    \\\\ \\wcline{\\cseplinethin}{2-11}'), nl(File),
	write(File,'     & \\ \\textsf{\\&}  & 1.00'), write_nondet_speedups(File,tak), nl(File),
	write(File,'    \\\\ \\whline{\\hseplinew}\\whline{\\hseplinew}'), nl(File),
	nl(File),
	write(File,'  \\end{tabular}'), nl(File),
	write(File,'  \\vspace{0.5em}'), nl(File),
	write(File,'  \\caption{Speedups obtained using unrestricted IAP, with \\&!/2 or \\&/2.}'), nl(File),
	write(File,'  \\label{tab:speedups-det-nondet}'), nl(File),
	write(File,'\\end{table}'), nl(File),
	close(File).

write_det_speedups(Stream, Key) :-
	between(1,8,N),
	current_fact(t_seq(Key,Seq)),
	current_fact(t_det(Key,Par,N)),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	write(Stream,'& '),
	write(Stream,Sp),
	write(Stream,' '),
	fail.
write_det_speedups(_,_).

write_nondet_speedups(Stream, Key) :-
	between(1,8,N),
	current_fact(t_seq(Key,Seq)),
	current_fact(t_nondet(Key,Par,N)),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	write(Stream,'& '),
	write(Stream,Sp),
	write(Stream,' '),
	fail.
write_nondet_speedups(_,_).
