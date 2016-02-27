:- module(unittest_base,
	    [
		atom_ref/2,
		empty_output/1,
		file_test_input/1,
		file_test_output/1,
		group_list/3,
		ref_atom/2,
		runner_global_file_name/1,
		tmp_dir/1,
		wrapper_file_name/3,
		unittest_print_clause/3,
		unittest_print_clauses/3
	    ],
	    [assertions, unittestprops]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(varnames(apply_dict))).

:- initialization(init_tmp_dir).

:- doc(author, "Edison Mera").

:- test group_list(A, B, C) : (A = [a, a, b, a, c, d, a, 1, 2, a, 1], B = [])
	=> ( C = [count(a, 5), count(b, 1), count(c, 1), count(d, 1),
		count(1, 2), count(2, 1)] ).

group_list([],    M,  M).
group_list([E|L], M0, M) :-
	add_to_list(M0, E, M1),
	!,
	group_list(L, M1, M).

:- test add_to_list(A, B, C) : (A = [count(b, 3), count(a, 1)], B = a)
	=> (C = [count(b, 3), count(a, 2)]).

:- export(add_to_list/3).

add_to_list([],               E, [count(E, 1)]).
add_to_list([count(E, N0)|M], E, [count(E, N)|M]) :-
	!,
	N is N0 + 1.
add_to_list([count(E, N)|M0], F, [count(E, N)|M]) :-
	!,
	add_to_list(M0, F, M).

% show_result_summary((Name=Value)) :-
% 	format("~w\t~w\n", [Name, Value]).

% show_results_summary(TestResultsSummary) :-
% 	display('Status\tTimes\n'),
% 	display('------- --------\n'),
% 	list(TestResultsSummary,show_result_summary).

% tmp_dir( '/tmp/ciaotest/').

:- data tmp_dir/1.

init_tmp_dir :-
	get_test_tmp_dir(TmpDir),
	retractall_fact(tmp_dir(_)),
	assertz_fact(tmp_dir(TmpDir)).

get_test_tmp_dir(TmpDir) :-
	mktemp_in_tmp('ciaotestXXXXXX', FileName),
	delete_file(FileName),
	atom_concat(FileName, '/', TmpDir).

file_test_output('test_output_auto.pl').
file_test_input('test_input_auto.pl').
runner_global_file_name('test_run_auto.pl').

wrapper_file_name(TmpDir, Module, WrapperFile) :-
	atom_concat([TmpDir, Module, '_wrp_auto.pl'],
	    WrapperFile).

ref_atom('$ref'(Ref1, Ref2), ARef) :-
	atom_number(ARef1, Ref1),
	atom_number(ARef2, Ref2),
	atom_concat(['ref', '_', ARef1, '_', ARef2], ARef).

atom_ref(ARef, '$ref'(Ref1, Ref2)) :-
	atom_concat(['ref', '_', ARef1, '_', ARef2], ARef),
	atom_number(ARef1, Ref1),
	atom_number(ARef2, Ref2).

empty_output(TmpDir) :-
	file_test_output(BOut),
	atom_concat(TmpDir, BOut, Out),
	string_to_file("", Out).

% unittest_print_clause(Term, S, _Dict) :-
% 	current_output(CO),
% 	set_output(S),
% 	writeq(Term),
% 	write('.'),
% 	nl,
% 	set_output(CO).

unittest_print_clause(Term, S, Dict) :-
	apply_dict(Term, Dict, ATerm),
	current_output(CO),
	set_output(S),
	writeq(ATerm),
	write('.'),
	nl,
	set_output(CO).
% 	portray_clause(S, ATerm).

unittest_print_clauses(Term, S, Dict) :-
	current_output(CO),
	set_output(S),
	list(Term, unittest_print_clause(S, Dict)),
	set_output(CO).
