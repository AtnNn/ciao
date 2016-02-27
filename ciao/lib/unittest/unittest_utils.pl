:- module(unittest_utils, [process_test_args/1, read_file_loop/2, testing/4],
	    [assertions, unittestprops]).

:- set_prolog_flag(write_strings, on).

:- use_module(library(aggregates)).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(unittest(unittest_base))).
:- use_module(library(rtchecks(rtchecks_utils))).

:- doc(author, "Edison Mera").

process_test_args([]).
process_test_args([load, Module|Args]) :-
	use_module(Module),
	process_test_args(Args).

read_file_loop(File, Term) :-
	open(File, read, SI),
	read_stream_loop(SI, Term).

read_stream_loop(SI, Term) :-
	repeat,
	(
	    read_data(SI, Term) ->
	    true
	;
	    !,
	    close(SI),
	    fail
	).

:- meta_predicate testing(?, ?, goal, goal).
testing(ARef, TmpDir, Precond, Pred) :-
	file_test_output(BOut),
	atom_concat(TmpDir, BOut, Out),
	testing_internal(Precond, Pred, Status),
	open(Out, append, IO),
	write_data(IO, test_output_db(ARef, Status)),
	close(IO).



:- data signals_db/1.

:- meta_predicate testing_internal(goal, goal, ?).
testing_internal(Precond, Pred, st(RTCErrors, Signals, Result)) :-
	retractall_fact(signals_db(_)),
	intercept(exec_test(Precond, Pred, Result),
	    E, assertz_fact(signals_db(E))),
	findall(E, retract_fact(signals_db(E)), Signals),
	load_rtchecks(RTCErrors).

:- meta_predicate exec_test(goal, goal, ?).
exec_test(Precond, Pred, Result) :-
	test_precondition_exception(
	    test_precondition(Precond,
		test_pred_exception(
		    test_postcondition(
			save_rtchecks(
			    test_result(Pred,
				Result)), Result), Result), Result), Result).

:- meta_predicate test_result(goal, ?).
test_result(Pred, Result) :-
	if(Pred, Result = true, Result = fail(predicate)).

:- meta_predicate test_postcondition(goal, ?).
test_postcondition(Pred, Result) :-
	catch(Pred, postcondition(PostEx),
	    (Result = exception(postcondition, PostEx))).

:- meta_predicate test_pred_exception(goal, ?).
test_pred_exception(Pred, Result) :-
	catch(Pred, PredEx, (Result = exception(predicate, PredEx))).

:- meta_predicate test_precondition(goal, goal, ?).
test_precondition(Precond, Pred, Result) :-
	Precond -> Pred ; Result = fail(precondition).

:- meta_predicate test_precondition_exception(goal, ?).
test_precondition_exception(Pred, Result) :-
	catch(Pred, PrecEx, (Result = exception(precondition, PrecEx))).
