:- module(debugger, [
% jf: remove these commented lines if everything is ok - 20031122
%	'$debugger_state'/2,'$debugger_mode'/0,'$spypoint'/3,
%	srcdbg_spy/7,
		call_in_module/2,
		debug_trace/1,
		debugging/0,
		do_interrupt_command/1,
		leash/1,
		maxdepth/1,
		switch_off_debugger/0,
		adjust_debugger/0,
		current_debugged/1,
% Common
		debug_module/1,
		debug_module_source/1,
		nodebug_module/1,
		nospy/1,
		spy/1],
	    [dcg, assertions, hiord, define_flag]).

:- use_module(engine(debugger_support)).
:- use_module(library(debugger(debugger_lib)), [
		all_spypoints/0,
		adjust_debugger_state/2,
		breakpoint/5,
		break_info/6,
		call_hook1/1,
		call_hook2/2,
		debugging_options/0,
		debugdepth/1,
		defaultopt/1,
		do_retry_fail/3,
		functor_spec/5,
		get_attributed_vars/3,
		get_command/1,
		get_debugger_state/1,
		lastof/3,
		leashed/1,
		multpredspec/1,
		nospy1/1,
		port_info/2,
		print_srcdbg_info/5,
		proc_extraopts/3,
		reset_debugger/1,
		reset_printdepth/0,
		retry_hook_/8,
		set_debugger/1,
		set_defaultopt/3,
		set_printdepth/1,
		show_leash_info/1,
		spy1/1,
		spy_info/5,
		what_is_on/1,
		write_goal2/4,
		write_goal_v/3,
		write_goal_v_name/3]).
:- reexport(library(debugger(debugger_lib)), [
		debug/0,
		nodebug/0,
		trace/0,
		notrace/0,
		get_debugger_state/1,
		nospyall/0,
		breakpt/6,
		nobreakpt/6,
		nobreakall/0,
		reset_debugger/1,
		retry_hook/4,
		list_breakpt/0]).
:- use_module(engine(internals)).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1, '$meta_call'/1]).
:- use_module(library(format)).
:- use_module(library(ttyout)).
:- use_module(library(read),   [read/2]).
:- use_module(library(aggregates)).
:- use_module(library(varnames(apply_dict))).
:- use_module(library(lists), [append/3]).
:- use_module(user,           ['$shell_call'/1]).

:- doc(title, "Predicates controlling the interactive debugger").

:- doc(module, "This library implements predicates which are
   normally used in the interactive top-level shell to debug
   programs. A subset of them are available in the embeddable debugger.").

:- doc(author, "A. Ciepielewski").
:- doc(author, "Mats Carlsson").
:- doc(author, "T. Chikayama").
:- doc(author, "K. Shen").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel C. Rodriguez").
:- doc(author, "Edison Mera").

:- doc(hide, adjust_debugger/0).
:- doc(hide, switch_off_debugger/0).
:- doc(hide, current_debugged/1).
:- doc(hide, reset_debugger/1).
:- doc(hide, set_debugger/1).
:- doc(hide, retry_hook/4).
:- doc(hide, debug_trace/1).
:- doc(hide, do_interrupt_command/1).

:- include(library(debugger(debugger_common))).


%------------------ Bug Comments ------------------------------

:- doc(bug, "Add an option to the emacs menu to automatically select
	all modules in a project.").
:- doc(bug, "Consider the possibility to show debugging messages 
	directly in the source code emacs buffer.").

%------------------Prolog debugger by AC------------------------------
% Minor hacks by MC.
% Some hacks by Takashi Chikayama (17 Dec 87)
%   - Making tracer to use "print" rather than "write"
%   - Temporarily switching debugging flag off while writing trace
%     message and within "break" level.
% Some hacks by Kish Shen (May 88)
%   - Subterm navigation
%   - Handle unbound arg in spy/1 and nospy/1
%   - Trap arith errors in debug mode
%------------- Built-in predicates for debugging------------------------

define_flag(check_cycles, [on, off], off).

% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

%%:- initialization(initialize_debugger_state).
%%:- on_abort(initialize_debugger_state).

% This has to be done before any choicepoint
% initialize_debugger_state used in internals.pl --EMM
:- entry initialize_debugger_state/0.
initialize_debugger_state :-
	'$debugger_state'(_, s(off, off, 1000000, 0, [])),
	'$debugger_mode'.

switch_off_debugger :-
	'$debugger_state'(State, State),
	'$setarg'(2, State, off, true),
	'$debugger_mode'.

:- pred maxdepth(MaxDepth) : int
# "Set maximum invocation depth in debugging to
           @var{MaxDepth}. Calls to compiled predicates are not included
           in the computation of the depth.".

maxdepth(D) :-
	integer(D), !,
	retractall_fact(debugdepth(_)),
	assertz_fact(debugdepth(D)),
	what_maxdepth.
maxdepth(D) :-
	format(user_error, '{Bad maxdepth ~q - must be an integer}~n', [D]).

:- prop port(X) + regtype.

port(call).
port(exit).
port(redo).
port(fail).

:- pred leash(Ports) : list(port)
# "Leash on ports @var{Ports}, some of @tt{call}, @tt{exit},
	@tt{redo}, @tt{fail}. By default, all ports are on leash.".

leash(L) :-
	nonvar(L),
	leash1(L),
	!.
leash(L) :-
	format(user_error, '{Bad leash specification ~q}~n', [L]).

leash1(half) :- !, leash1([call, redo]).
leash1(full) :- !, leash1([call, exit, redo, fail]).
leash1(loose) :- !, leash1([call]).
leash1(none) :- !, leash1([]).
leash1(tight) :- !, leash1([call, redo, fail]).
leash1(L) :-
	list(L),
	retractall_fact(leashed(_)), leashlist(L), what_is_leashed.

leashlist([]).
leashlist([Port|L]) :-
	assertz_fact(leashed(Port)),
	leashlist(L).

%------------------------ meta-interpreters ------------------------------

% called from interpreter.pl

debug_trace(X) :-
	extract_info(X, Goal, Pred, Src, Ln0, Ln1, Dict, Number),
	( debuggable(Goal) ->
	    get_debugger_state(State),
	    debug_trace2(Goal, State, Pred, Src, Ln0, Ln1, Dict, Number)
	;
	    term_to_meta(X, G),
	    '$nodebug_call'(G)
	).

debuggable(Goal) :-
	in_debug_module(Goal).
debuggable(_) :-
	get_debugger_state(S),
	arg(5, S, [a(_, Ancestor, _, _)|_]),
	in_debug_module(Ancestor).

in_debug_module(G) :-
	functor(G, F, _),
	current_fact(debug_mod(_, Mc)),
	atom_concat(Mc, _, F).

debug_call(Goal) :- '$shell_call'(Goal).

%-------------------------facilities-------------------------------------

% extract_info('debugger:srcdbg_spy'(Goal,Pred,Src,Ln0,Ln1,Dict,Number),
extract_info('debugger_support:srcdbg_spy'(Goal, Pred, Src, Ln0, Ln1, Dict,
		Number), NewGoal, Pred, Src, Ln0, Ln1, Dict, Number) :-
	!,
	term_to_meta(NewGoal, Goal).
extract_info(Goal, Goal, nil, nil, nil, nil, d([], []), nil).

adjust_debugger :-
	get_debugger_state(State),
	arg(1, State, G),
	adjust_debugger_state(State, G).

get_attributed_vars(Term, AtVars) :-
	current_prolog_flag(check_cycles, Flag),
	( Flag = on ->
	    get_attributed_vars_cy(Term, [], _, [], AtVars)
	;
	    get_attributed_vars(Term, AtVars, [])
	).

get_attributed_vars_cy(X, Seen, Seen, At, NewAt) :-
	var(X), !,
	( get_attribute(X, AtX) ->
	    NewAt = [attach_attribute(X, AtX)|At]
	;
	    NewAt = At
	).
get_attributed_vars_cy(X, Seen, Seen, At, At) :-
	atomic(X), !.
get_attributed_vars_cy(X, Seen, Seen, At, At) :-
	already_seen(Seen, X), !.
get_attributed_vars_cy(X, Seen, NewSeen, At, NewAt) :-
	functor(X, _, Ar),
	get_attributed_vars_cy_args(Ar, X, [X|Seen], NewSeen, At, NewAt).

get_attributed_vars_cy_args(0, _, Seen,  Seen,  At,  At) :- !.
get_attributed_vars_cy_args(N, X, Seen0, Seen2, At0, At2) :-
	N > 0,
	arg(N, X, A),
	get_attributed_vars_cy(A, Seen0, Seen1, At0, At1),
	N1 is N - 1,
	get_attributed_vars_cy_args(N1, X, Seen1, Seen2, At1, At2).

already_seen([T|_Ts], Term) :-
	T == Term,
	!.
already_seen([_T|Ts], Term) :- already_seen(Ts, Term).

do_interrupt_command(0'@) :- !, % @(command)
	ttyskipeol, do_once_command('| ?- '),
	do_interrupt_command(0'\n).
do_interrupt_command(0'a) :- !, % a(bort)
	ttyskipeol, abort.
% do_interrupt_command(0'b) :- !, % b(reak)
% 	ttyskipeol, break.
do_interrupt_command(0'c) :- !, % c(ontinue)
	ttyskipeol.
do_interrupt_command(0'd) :- !, % d(ebug)
	ttyskipeol, debug.
do_interrupt_command(0'e) :- !, % e(xit)
	ttyskipeol, halt.
do_interrupt_command(0't) :- !, % t(race)
	ttyskipeol, trace.
do_interrupt_command(0'\n) :- !, % cr
	format(user, '~nCiao interruption (h for help)? ', []),
	ttyflush,
	ttyget(C),
	do_interrupt_command(C).
do_interrupt_command(_) :- % h(elp) or other
	ttyskipeol,
	interrupt_options,
	do_interrupt_command(0'\n).

interrupt_options :-
	ttynl,
	ttydisplay('Ciao interrupt options:'), ttynl,
	ttydisplay('    a        abort           - cause abort'), ttynl,
%	ttydisplay('    b        break           - cause break'), ttynl,
	ttydisplay('    c        continue        - do nothing'), ttynl,
	ttydisplay('    d        debug           - start debugging'), ttynl,
	ttydisplay('    t        trace           - start tracing'), ttynl,
	ttydisplay('    e        exit            - cause exit'), ttynl,
	ttydisplay('    @        command         - execute a command'), ttynl,
	ttydisplay('    h        help            - get this list'), ttynl.

% :- meta_predicate call_in_module(?, fact).

:- pred call_in_module(Module, Predicate) : atm * callable
# "Calls predicate @var{Predicate} belonging to module
	@var{Module}, even if that module does not export the
	predicate. This only works for modules which are in debug 
	(interpreted) mode (i.e., they are not optimized).".

call_in_module(Module, Goal) :-
	module_concat(Module, Goal, MGoal),
	'$meta_call'(MGoal).
