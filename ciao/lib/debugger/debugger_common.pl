:- use_module(library(varnames(complete_dict)), [set_undefined_names/3]).

debug_trace2(X, State, Pred, Src, L0, L1, d(UDict, CDict), Number) :-
	append(UDict, CDict, Dict0),
	set_undefined_names(Dict0, 1, _),
	select_applicable(X, Dict0, ADict),
	Dict = d(UDict, CDict, ADict),
	retry_hook_(X, B, D, NA, OA, Port, State, Dict),
	'$setarg'(4, State, B,  on),
	'$setarg'(5, State, NA, on),
	(
	    Port = call,
	    '$metachoice'(C0),
	    call_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number),
	    '$metachoice'(C1)
	; fail_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number), !, fail
	),
	( exit_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number)
	; redo_hook(X, B, D, State, Pred, Src, L0, L1, Dict, Number), fail
	),
%	Remove choicepoints in deterministic goals to speed up debugging -- EMM
	(C0 == C1 -> ! ; true),
	'$setarg'(5, State, OA, on).

call_hook(X, B, _, State, _, _, _, _, _, _) :-
	arg(3, State, Level),
	B>Level, !,
	call_hook1(X).
call_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
	debug_port(X, B, D, call, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number),
	call_hook2(Msg, X).

exit_hook(_, B, _, State, _, _, _, _, _, _) :-
	arg(3, State, Level), B>Level, !.
exit_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, exit, State, _, Pred, Src, Ln0, Ln1, Dict, Number).

redo_hook(_, B, _, State, _, _, _, _, _, _) :-
	arg(3, State, Level), B>Level, !.
redo_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
	debug_port(X, B, D, redo, State, _, Pred, Src, Ln0, Ln1, Dict, Number).

fail_hook(_, B, _, State, _, _, _, _, _, _) :-
	arg(3, State, Level), B>Level, !.
fail_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Dict, Number) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, fail, State, _, Pred, Src, Ln0, Ln1, Dict, Number).

debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :-
	(
	    '$spypoint'(X, on, on)
	;
% Ln0 is free because there is no way to determine where the 
% clause start, but the end of the clause can be determine exactly.
	    current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
	),
	!,
	defaultopt(Op),
	prompt_command(Op, X, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict, Number) :-
	arg(2, State, trace),
	current_fact(leashed(Port)), !,
	defaultopt(Op),
	prompt_command(Op, X, [], B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
debug_port(X, B, D, Port, State, no, Pred, Src, Ln0, Ln1, Dict, Number) :-
	arg(2, State, trace), !,
	defaultopt(Op),
	write_goal(Op, X, [], B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number),
	ttynl.
debug_port(_, _, _, _, _, no, _, _, _, _, _, _).

prompt_command(T, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number),
	get_command(C),
	do_trace_command(C, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).

do_trace_command(0'a, _, _, _, _, _, _, _, _, _, _, _, _, _) :- !, % a(bort)
	abort.
do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !, % c(reep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'\n, _, _, _, _, _, State, no, _, _, _, _, _, _) :-
	!, % CR (creep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :- !, % d(isplay)
	set_defaultopt(0'd, false, false),
	prompt_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command([0'd, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number) :- % d(isplay)
	proc_extraopts(AV, A, V), !,
	set_defaultopt(0'd, A, V),
	prompt_command(0'd, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'g, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :- !, % g(ancestors)
	arg(5, State, CA),
	show_ancestors(CA, -1),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command([0'g, Arg], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number) :- !, % g(ancestors) arg
	arg(5, State, CA),
	show_ancestors(CA, Arg),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'l, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !, % l(eap)
	'$setarg'(2, State, debug, true),
	'$debugger_mode'.
do_trace_command(0'n, _, _, _, _, _, State, no, _, _, _, _, _, _) :-
	!, % n(odebug)
% 	nodebug.
	'$setarg'(3, State, 0, true).
do_trace_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :- !, % p(rint)
	set_defaultopt(0'p, false, false),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command([0'p, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number) :- % p(rint)
	proc_extraopts(AV, A, V), !,
	set_defaultopt(0'p, A, V),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'v, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :- !, % v(ariables)
	prompt_command(0'v, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command([0'v, Name], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number) :- !, % v(ariables)
	prompt_command([0'v, Name], X, Xs, B, D, Port, State, Msg, Pred, Src,
	    Ln0, Ln1, Dict, Number).
do_trace_command(0'r, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !, % r(etry)
	arg(5, State, [a(B, _, _, _)|_]),
	do_retry_fail(B, State, call).
do_trace_command([0'r, B], _, _, _, _, _, State, no, _, _, _, _, _, _) :-
	!, % r(etry) arg
	do_retry_fail(B, State, call).
do_trace_command(0'f, _, _, _, _, _, State, no, _, _, _, _, _, _) :- !, %f(ail)
	arg(5, State, [a(B, _, _, _)|_]),
	do_retry_fail(B, State, fail).
do_trace_command([0'f, B], _, _, _, _, _, State, no, _, _, _, _, _, _) :-
	!, % f(ail) arg
	do_retry_fail(B, State, fail).
do_trace_command(0's, _, _, B, _, Port, State, no, _, _, _, _, _, _) :- % s(kip)
	set_skip(Port, B, State), !.
do_trace_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, % w(rite)
	set_defaultopt(0'w, false, false),
	prompt_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command([0'w, AV], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number) :-
	proc_extraopts(AV, A, V), !,
	set_defaultopt(0'w, A, V),
	prompt_command(0'w, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'+, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, % +(spy this)
	lastof(Xs, _-X, _-Goal),
	spy1(Goal),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'-, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, % -(nospy this)
	lastof(Xs, _-X, _-Goal),
	nospy1(Goal),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'=, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, % =(debugging)
	reset_debugger(_),
	debugging,
	set_debugger(State),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
% do_trace_command(0'b, X, Xs, B, D, Port, State, Msg) :- !, % b(reak)
% 	break,
% 	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'@, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, %@ (command)
	do_once_command('| ?- '),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command(0'u, _, _, _, _, call, _, answer(X1), _, _, _, _, _, _) :-
	!, %u (unify)
	'$prompt'(Old, '|: '),
	read(user, X1),
	'$prompt'(_, Old).
do_trace_command(0'<, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, %< (reset printdepth)
	reset_printdepth,
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).
do_trace_command([0'<, I], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number) :-
	!, %< arg (set printdepth)
	set_printdepth(I),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'^, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, %^ (reset subterm)
	lastof(Xs, _-X, _-Goal),
	defaultopt(Op),
	prompt_command(Op, Goal, [], B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'^, 0], _, [_-X|Xs], B, D, Port, State, Msg, Pred, Src, Ln0,
	    Ln1, Dict, Number) :-
	!, %^ 0 (up subterm)
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command([0'^, I], X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number) :- %^ arg (set subterm)
	arg(I, X, Ith), !,
	defaultopt(Op),
	prompt_command(Op, Ith, [I-X|Xs], B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'?, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, % ?(help)
	debugging_options,
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(0'h, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Dict,
	    Number) :-
	!, % h(elp)
	debugging_options,
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number).
do_trace_command(_, X, Xs, B, D, Port, State, Msg,
	    Pred, Src, Ln0, Ln1, Dict, Number) :- % all others
	format(user, '{Option not applicable at this port}~n', []),
	defaultopt(Op),
	prompt_command(Op, X, Xs, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1,
	    Dict, Number).

write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Dict, Number) :-
	reset_debugger(State),
	port_info(Port, Pport),
	current_output(CO),
	set_output(user),
	do_write_goal(T, X, Xs, B, D, Pport, Pred, Src, Ln0, Ln1, Dict,
	    Number),
	set_output(CO),
	set_debugger(State).

do_write_goal(0'v, X, _, _, _, _, _, _, _, _, Dict, _) :-
	!,
	write_goal_v(X, Dict, get_attributed_vars).
do_write_goal([0'v, SName], _, _, _, _, _, _, _, _, _, Dict, _) :-
	!,
	write_goal_v_name(SName, Dict, get_attributed_vars).
do_write_goal(T, X, Xs, B, D, Pport, Pred, Src, Ln0, Ln1, Dict, Number) :-
	print_srcdbg_info(Pred, Src, Ln0, Ln1, Number),
	spy_info(Xs, X, Mark0, S, []),
	( Mark0 == '   ' -> break_info(Pred, Src, Ln0, Ln1, Number, Mark)
	; Mark=Mark0
	),
	display_list([Mark, B, '  ', D, Pport|S]),
	get_attributed_vars(X, AtVars),
	write_goal2(T, X, Dict, AtVars).

do_once_command(Prompt) :-
	'$prompt'(OldPrompt, Prompt),
	reset_debugger(State),
	read(user, Command),
	'$prompt'(_, '|: '),
	( debug_call(Command) -> Y=yes
	; Y=no
	),
	'$prompt'(_, OldPrompt),
	set_debugger(State),
	Y=yes, !.
do_once_command(_) :-
	format(user_error, '{Warning: goal failed}~n', []).

set_skip(call, To, State) :- '$setarg'(3, State, To, true).
set_skip(redo, To, State) :- '$setarg'(3, State, To, true).
set_skip(_,    _,  State) :-
	format(user, '{Skip not applicable at this port, creeping ...}~n', []),
	do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _, _).

show_ancestors([_], _) :- !,
	ttynl, ttydisplay('No ancestors.'), ttynl.
show_ancestors([_|CA], N) :-
	ttynl, ttydisplay('Ancestors:'), ttynl,
	list_ancestors(CA, N).

list_ancestors([],                    _) :- !.
list_ancestors(_,                     0) :- !.
list_ancestors([a(B, X, D, Dict)|As], N0) :-
	N is N0-1,
	list_ancestors(As, N),
	defaultopt(Op),
	write_goal(Op, X, [], B, D, void, _Pred, _Src, nil, nil, Dict, nil),
	ttynl.

:- pred debugging/0 # "Display debugger state.".

debugging :-
	get_debugger_state(State),
	arg(1, State, G),
	what_is_on(G),
	what_is_debugged,
	what_is_leashed,
	what_maxdepth,
	all_spypoints,
	ttynl.

what_is_debugged :-
	current_debugged(Ms),
	( Ms = [] ->
	  format(user, '{No module is selected for debugging}~n', [])
	; format(user, '{Modules selected for debugging: ~w}~n', [Ms])
	).

what_is_leashed :-
	is_leashed([call, exit, redo, fail], L),
	show_leash_info(L).

is_leashed([],     []).
is_leashed([X|Xs], [X|Ys]) :- current_fact(leashed(X)), !, is_leashed(Xs, Ys).
is_leashed([_|Xs], Ys) :- is_leashed(Xs, Ys).

what_maxdepth :-
	current_fact(debugdepth(M)),
	format(user, '{Interpreter maxdepth is ~w}~n', [M]).

current_debugged(Ms) :- findall(M, current_fact(debug_mod(M, _)), Ms).

:- data debug_mod/2.

:- pred debug_module(Module) : atm(Module)
# "The debugger will take into acount module @var{Module}
          (assuming it is loaded in interpreted mode).  When issuing this
          command at the toplevel shell, the compiler is instructed also
          to set to @em{interpret} the loading mode of files defining that
          module and also to mark it as 'modified' so that (re)loading 
	  this file or a main file that uses this module will force it 
	  to be reloaded for source-level debugging.".

debug_module(M) :- atom(M), !,
	( current_fact(debug_mod(M, _)) ->
	    true
	; atom_concat(M, ':', Mc),
	    assertz_fact(debug_mod(M, Mc))
	).
debug_module(M) :-
	format(user_error, '{Bad module ~q - must be an atom}~n', [M]).

:- pred nodebug_module(Module) : atm(Module)
# "The debugger will not take into acount module @var{Module}.
          When issuing this command at the toplevel shell, the compiler is
          instructed also to set to @em{compile} the loading mode of files
          defining that module.".

nodebug_module(M) :- % If M is a var, nodebug for all
	retractall_fact(debug_mod(M, _)).
%        what_is_debugged.

%% This entry point is only for documentation purposes.
:- pred debug_module_source(Module) : atm(Module)
# "The debugger will take into acount module @var{Module}
	  (assuming it is is loaded in source-level debug mode).  When 
	  issuing this command at the toplevel shell, the compiler is 
	  instructed also to set to @em{interpret} the loading mode of 
	  files defining that module and also to mark it as 'modified'
	  so that (re)loading this file or a main file that uses this
	  module will force it to be reloaded for source-level debugging.".

debug_module_source(M) :-
	debug_module(M).

:- pred spy(PredSpec) : sequence(multpredspec)
# "Set spy-points on predicates belonging to debugged modules and
	  which match @var{PredSpec}, switching the debugger on if
          needed. This predicate is defined as a prefix operator by the
          toplevel.".

spy(Preds) :-
	get_debugger_state(State),
	(arg(1, State, off) -> debug ; true),
	parse_functor_spec(Preds, X, spy1(X)).

:- pred nospy(PredSpec) : sequence(multpredspec)
# "Remove spy-points on predicates belonging to debugged modules
          which match @var{PredSpec}. This predicate is defined as a prefix
          operator by the toplevel.".

nospy(Preds) :-
	parse_functor_spec(Preds, X, nospy1(X)).

:- meta_predicate parse_functor_spec(?, ?, goal).

parse_functor_spec(V, _, _) :-
	var(V), !,
	format(user_error, '{A variable is a bad predicate indicator}~n', []).
parse_functor_spec((S, Ss), GoalArg, Goal) :-
	parse_functor_spec(S,  GoalArg, Goal),
	parse_functor_spec(Ss, GoalArg, Goal).
parse_functor_spec(S, GoalArg, Goal) :-
	Flag=f(0),
	( functor_spec(S, Name, Low, High, M),
	    current_fact(debug_mod(M, Mc)),
	    atom_concat(Mc, Name, PredName),
	    '$current_predicate'(PredName, GoalArg),
	    functor(GoalArg, _, N),
	    N >= Low, N =< High,
	    '$setarg'(1, Flag, 1, true),
	    '$nodebug_call'(Goal),
	    fail
	; Flag=f(0),
	    format(user_error,
		"{Bad predicate indicator or predicate undefined "||
		"in modules currently debugged:~n ~w}~n", [S]),
	    fail
	; true
	).
