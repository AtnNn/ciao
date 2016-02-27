:- module(_, _, [assertions, nativeprops, regtypes]).

:- use_module(library(file_utils)).
:- use_module(library(format)).
:- use_module(library(terms)).
:- use_module(library(write)).
:- use_module(library(lists)).

:- include('../benchcommon/_all/config').
:- include('../estimatecommon/_all/solution').

evaluate(A, N) :-
	atom(A),
	!,
	read_value(A, N).
evaluate(A, A) :-
	number(A),
	!.
evaluate(- B, N) :-
	evaluate(B, BN),
	N is - BN.
evaluate(A + B, N) :-
	evaluate(A, AN),
	evaluate(B, BN),
	N is AN + BN.
evaluate(A - B, N) :-
	evaluate(A, AN),
	evaluate(B, BN),
	N is AN - BN.
evaluate(A * B, N) :-
	evaluate(A, AN),
	evaluate(B, BN),
	N is AN - BN.
evaluate(A / B, N) :-
	evaluate(A, AN),
	evaluate(B, BN),
	N is AN - BN.

:- data base_dir/1.

% base_dir( '/home/edison/apps/cpp/miniprolog/bench/_i486-linux-gnu/' ).
% base_dir( '/home/edison/apps/cpp/miniprolog/bench/_arm-none-linux-gnueabi/' ).

read_value(A, N) :-
	repetitions(time, R, _),
	base_dir(Dir),
	atom_number(AR, R),
	atom_concat([Dir, A, '_0_time_c.oti'],        File0),
	atom_concat([Dir, A, '_', AR, '_time_c.oti'], FileN),
	file_to_string(File0, Oti0),
	number_codes(NOti0, Oti0),
	file_to_string(FileN, OtiN),
	number_codes(NOtiN, OtiN),
	N is NOtiN - NOti0.

calibrate_each(B = E, B = N) :-
	evaluate(E, N0),
	N is N0.

calibrate([],     []).
calibrate([X|Xs], [C|Cs]) :-
	calibrate_each(X, C),
	calibrate(Xs, Cs).

bytecode_costs(C) :-
	solution([X]),
	calibrate(X, C).

number_fail('0', succ).
number_fail('1', fail).

number_fail_a('0', not_fails).
number_fail_a('1', fails).

:- use_module('../bin/miniprolog_auto').

dump_timing_model(prolog) :-
	write('% -*- mode: ciao; -*-'), nl,
	bytecode_costs(Cs),
	repetitions(time, R, _),
	(
	    member(B = C, Cs),
	    atom_concat([Name, '_', N, '_', AIdParams], B),
	    atom_number(AIdParams, IdParams),
	    number_fail_a(N, FailA),
	    ( bc_params(Name, Params) ->
		IdParams1 is IdParams + 1,
		nth(IdParams1, Params, Param),
		(
		    member(Args, Param),
		    WamCode =.. [Name|Args],
		    format(
			":- true pred ~w\n\t+ ( cost(ub, exectime, ~w),\n"
			|| "\t    cost(lb, exectime, ~w),\n"
			|| "\t    cost(me, exectime, ~w),\n\t    ~w ).\n",
			[WamCode, C / R, C / R, C / R, FailA]),
		    fail
		;
		    true
		)
	    ;
		bytecode(Name, Arity),
		format(
		    ":- true pred ~w/~w\n\t+ ( cost(ub, exectime, ~w),\n"
		    || "\t    cost(lb, exectime, ~w),\n"
		    || "\t    cost(me, exectime, ~w),\n\t    ~w ).\n",
		    [Name, Arity, C / R, C / R, C / R, FailA])
	    ),
	    fail
	;
	    true
	).
dump_timing_model(text) :-
	bytecode_costs(Cs),
	length(Cs, M),
	display(M), nl,
	repetitions(time, R, _),
	(
	    member(B = C, Cs),
	    atom_concat([Name, '_', N, '_', IdParams], B),
	    number_fail(N, _Fail),
	    display_list([Name, '\t', N, '\t', IdParams, '\t', C, '\t',
		    R]),
	    nl,
	    fail
	;
	    true
	).


main([Command, BaseDir]) :-
	assertz_fact(base_dir(BaseDir)),
	dump_timing_model(Command).
