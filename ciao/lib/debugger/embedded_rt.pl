:- module(embedded_rt, [
		srcdbg_byrd/7,
		spy/1,
		trace/0,
		debug/0,
		notrace/0,
		nodebug/0,

		debug_mod/2,

		debug_module/1,
		debug_module_source/1,
		nodebug_module/1,
		nospy/1,
		spy/1],
	    [dcg, assertions]).

:- use_module(engine(internals), ['$prompt'/2,
		term_to_meta/2, '$setarg'/4, '$current_predicate'/2]).
:- use_module(engine(debugger_support), ['$spypoint'/3, '$debugger_mode'/0]).
:- use_module(engine(hiord_rt),         ['$nodebug_call'/1]).
:- use_module(library(ttyout),          [ttynl/0, ttydisplay/1]).
:- use_module(library(read),            [read/2]).
:- use_module(library(lists),           [append/3]).
:- use_module(library(debugger(debugger_lib)), [
		all_spypoints/0,
		call_hook1/1,
		call_hook2/2,
		breakpoint/5,
		break_info/6,
		debugging_options/0,
		defaultopt/1,
		do_retry_fail/3,
		functor_spec/5,
		debugdepth/1,
		get_attributed_vars/3,
		get_command/1,
		get_debugger_state/1,
		lastof/3,
		leashed/1,
		multpredspec/1,
		nospy1/1,
		proc_extraopts/3,
		reset_debugger/1,
		reset_printdepth/0,
		retry_hook_/8,
		port_info/2,
		print_srcdbg_info/5,
		set_debugger/1,
		set_defaultopt/3,
		set_printdepth/1,
		show_leash_info/1,
		spy1/1,
		spy_info/5,
		what_is_on/1,
		write_goal2/4,
		write_goal_v/3,
		write_goal_v_name/3,
		debug/0,
		nodebug/0,
		notrace/0,
		trace/0]).
% :- use_module(library(debugger(debugger_rt)), [
% 		breakpoint/5,
% 		debug_mod/2,
% 		leashed/1
% 	    ]).
:- use_module(library(varnames(apply_dict))).
:- use_module(library(aggregates)).
:- use_module(library(format)).

:- redefining(trace/0).
:- redefining(debug/0).
:- redefining(notrace/0).
:- redefining(nodebug/0).

:- include(library(debugger(debugger_common))).
% initialize_srcdebugger :-
% 	format(user_error,'Point 1~n',[]),
% 	set_prolog_flag(embedded_debugger,off),
% 	format(user_error,'Point 2~n',[]),
% 	what_is_on(off).

% Predicates to interact with the debugger

trace :- debugger_lib:trace.
debug :- debugger_lib:debug.
notrace :- debugger_lib:notrace.
nodebug :- debugger_lib:nodebug.

% The embedded debugger
:- meta_predicate srcdbg_byrd(goal, _, _, _, _, _, _).
srcdbg_byrd(X, Pred, Src, L0, L1, Dict, Number) :-
	get_debugger_state(State),
	arg(2, State, Debugging),
	( debuggable(Debugging, X, Pred, Src, L0, L1, Number) ->
	    term_to_meta(X1, X),
	    debug_trace2(X1, State, Pred, Src, L0, L1, Dict, Number)
	; '$nodebug_call'(X)
	).

debuggable(trace, _, _,    _,   _, _,   _).
debuggable(debug, X, Pred, Src, _, Ln1, Number) :-
	(
	    term_to_meta(G, X),
	    '$spypoint'(G, on, on)
	;
% Ln0 is free because there is no way to determine where the 
% clause start, but the end of the clause can be determine exactly.
	    current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
	),
	!.

debug_call(Goal) :- catch(Goal, E, error(['Thrown error ', E])).

get_attributed_vars(Term, AtVars) :-
	get_attributed_vars(Term, AtVars, []).
