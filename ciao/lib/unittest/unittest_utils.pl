:- module(unittest_utils, [process_test_args/1, read_file_loop/2, testing/4],
	    [assertions, unittestprops]).

:- set_prolog_flag(write_strings, on).

:- use_module(library(aggregates)).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(read)).
:- use_module(library(unittest(unittest_base))).
:- use_module(library(rtchecks(rtchecks_utils))).

:- doc(author, "Edison Mera").

process_test_args([]).
process_test_args(['-l', Module|Args]) :-
	use_module(Module),
	process_test_args(Args).

read_file_loop(File, Term) :-
	open(File, read, SI),
	read_stream_loop(SI, Term).

read_stream_loop(SI, Term) :-
	repeat,
	read(SI, Term0),
	(
	    Term0 == end_of_file ->
	    !,
	    close(SI),
	    fail
	;
	    Term = Term0
	).

:- meta_predicate testing(?, ?, goal, goal).
testing(ARef, TmpDir, Precond, Pred) :-
	file_test_output(BOut),
	atom_concat(TmpDir, BOut, Out),
	testing_internal(Precond, Pred, Status),
	open(Out, append, IO),
	unittest_print_clause(test_output_db(ARef, Status), IO, []),
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
	catch((
		Precond ->
		catch(catch(save_rtchecks(
			    if(
				Pred,
				Result = true,
				Result = fail(predicate)
			    )
			),
			postcondition(PostEx),
			(Result = exception(postcondition, PostEx))),
		    PredEx, (Result = exception(predicate, PredEx)))
	    ;
		Result = fail(precondition)
	    ),
	    PrecEx, (Result = exception(precondition, PrecEx))).
