:- module(debugger_lib, [
		all_spypoints/0,
		adjust_debugger_state/2,
		breakpoint/5,
		breakpt/6,
		break_info/6,
		call_hook1/1,
		call_hook2/2,
		debug/0,
		debugging_options/0,
		debugger_setting/2,
		defaultopt/1,
		debugdepth/1,
		display_nv0/3,
		display_nvs/3,
		do_retry_fail/3,
		functor_spec/5,
		get_attributed_vars/3,
		get_command/1,
		get_debugger_state/1,
		instantiated/1,
		lastof/3,
		leashed/1,
		list_breakpt/0,
		multpredspec/1,
		nobreakall/0,
		nobreakpt/6,
		nodebug/0,
		nospyall/0,
		nospy1/1,
		notrace/0,
		port_info/2,
		print_attributes/3,
		print_srcdbg_info/5,
		printdepth/1,
		proc_extraopts/3,
		reset_debugger/1,
		reset_printdepth/0,
		retry_hook_/8,
		retry_hook/4,
		set_debugger/1,
		set_defaultopt/1,
		set_defaultopt/3,
		set_printdepth/1,
		show_leash_info/1,
		spy1/1,
		spy_info/5,
		spypoint/1,
		trace/0,
		what_is_on/1,
		write_goal2/4,
		write_goal_v/3,
		write_goal_v_name/3
	    ],
	    [assertions, dcg, hiord]).

:- use_module(engine(debugger_support)).
:- use_module(library(ttyout)).
:- use_module(engine(internals), ['$predicate_property'/3,
		'$setarg'/4, term_to_meta/2, '$current_predicate'/2]).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(write)).
:- use_module(library(hiordlib)).
:- use_module(library(apply)).
:- use_module(library(sort)).
:- use_module(library(system), [cyg2win/3, using_windows/0]).
:- use_module(library(varnames(apply_dict))).

:- doc(hide, get_debugger_state/1).
:- doc(hide, what_is_on/1).
:- doc(hide, debugging_options/1).
:- doc(hide, spy1/1).
:- doc(hide, nospy1/1).
:- doc(hide, functor_spec/5).

% :- multifile define_flag/3.
% 
% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

/*
reset_debugger(State) :-
	'$debugger_state'(State, s(off,off,1000000,0,[])),
	'$debugger_mode'.
*/

get_command(Command) :-
	ttydisplay(' ? '),
	ttyflush,
	ttyget(C1),
	get_rest_command(C1, Command).

get_rest_command(0'\n, 0'\n) :- !.
get_rest_command(C1,   Command) :-
	ttyget(C2),
	get_arg(C2, C1, Command).

get_arg(0'\n, C,  C) :- !.
get_arg(C2,   C1, [C1, Arg]) :-
	C2 >= 0'0, C2 =< 0'9, !,
	trd_digits(C2, 0, Arg).
get_arg(0' , C1, C) :-
	ttyget(C2),
	get_arg(C2, C1, C).
get_arg(C2, C1, [C1, Arg]) :-
	trd_string(C2, Arg, "").

trd_digits(Ch, SoFar, I) :-
	Ch >= 0'0, Ch =< 0'9, !,
	Next is SoFar*10 + Ch - 0'0,
	ttyget(Ch1),
	trd_digits(Ch1, Next, I).
trd_digits(0'\n, I, I) :- !.
trd_digits(_,    I, J) :-
	ttyget(Ch),
	trd_digits(Ch, I, J).

trd_string(0'\n, I,       I) :- !.
trd_string(Ch,   [Ch|I0], I) :-
	ttyget(Ch1),
	trd_string(Ch1, I0, I).

mode_message(debug,
'{The debugger will first leap -- showing spypoints and breakpoints (debug)}').
mode_message(trace,
	    '{The debugger will first creep -- showing everything (trace)}').
mode_message(off,
	    '{The debugger is switched off}').

what_is_on(Mode) :-
	mode_message(Mode, Msg),
	ttydisplay(Msg),
	ttynl.

:- pred printopts(DefaultOption, Depth, Attribs, Vars).

:- data printopts/4.

printopts(0'p, 10, true, false).

printdepth(Depth) :- current_fact(printopts(_, Depth, _, _)).

defaultopt(O) :- current_fact(printopts(O, _, _, _)).

reset_printdepth :-
	set_printdepth(10).

set_printdepth(D) :-
	retract_fact(printopts(O, _, A, V)),
	assertz_fact(printopts(O, D, A, V)).

set_defaultopt(O) :-
	retract_fact(printopts(_, D, A, V)),
	assertz_fact(printopts(O, D, A, V)).

proc_extraopts("",   false, false).
proc_extraopts("a",  true,  false).
proc_extraopts("av", true,  true).
proc_extraopts("v",  false, true).
proc_extraopts("va", true,  true).

set_defaultopt(O, A, V) :-
	retract_fact(printopts(_, D, _, _)),
	assertz_fact(printopts(O, D, A, V)).

get_attributed_vars(X, At,                            At) :- atomic(X), !.
get_attributed_vars(X, [attach_attribute(X, AtX)|At], At) :-
	var(X),
	get_attribute(X, AtX), !.
get_attributed_vars(X,      At,  At) :- var(X), !. % No attributes
get_attributed_vars([X|Xs], At0, At2) :- !,
	get_attributed_vars(X,  At0, At1),
	get_attributed_vars(Xs, At1, At2).
get_attributed_vars(X, At0, At1) :-
	functor(X, _, Ar),
	get_attributed_vars_args(Ar, X, At0, At1).

get_attributed_vars_args(0, _, At,  At) :- !.
get_attributed_vars_args(N, X, At0, At2) :-
	N > 0,
	arg(N, X, A),
	get_attributed_vars(A, At0, At1),
	N1 is N - 1,
	get_attributed_vars_args(N1, X, At1, At2).

print_attributes(As, Op, WriteOpts) :-
	list(As, print_attribute(Op, WriteOpts)).

print_attribute(A, Op, WriteOpts) :-
	nl,
	tab(10), % 10 blanks
	display('['),
	write_op(A, Op, WriteOpts),
	display(']').

% Command options
debugging_options :-
	ttydisplay('Debugging options:'), ttynl,
	ttydisplay('   <cr>    creep            c      creep'), ttynl,
	ttydisplay('    l      leap             s      skip'), ttynl,
	ttydisplay('    r      retry            r <i>  retry i'), ttynl,
	ttydisplay('    f      fail             f <i>  fail i'), ttynl,
	ttydisplay('    d <av> display av       p <av> print av'), ttynl,
	ttydisplay('    w <av> write av         a      abort'), ttynl,
	ttydisplay('    v      variables        v <N>  variable N'), ttynl,
	ttydisplay('    g      ancestors        g <n>  ancestors n'), ttynl,
	ttydisplay('    n      nodebug          =      debugging'), ttynl,
	ttydisplay('    +      spy this         -      nospy this'), ttynl,
	ttydisplay('    @      command          u      unify'), ttynl,
	ttydisplay('    <      reset printdepth < <n>  set printdepth'), ttynl,
	ttydisplay('    ^      reset subterm    ^ <n>  set subterm'), ttynl,
	ttydisplay('    ?      help             h      help'), ttynl,
	ttynl,
	ttydisplay('Note: In d, p and w options, you can add'), ttynl,
	ttydisplay('  <a> to show attributes and <v> to show variables.'),
	ttynl,
	ttynl.

display_nv0(Name=Value, Op, WO) :-
	display_list(['\t   ', Name, ' = ']),
	write_op(Op, Value, WO).

display_nv(NameValue, Op, WO) :-
	display(','), nl,
	display_nv0(NameValue, Op, WO).

display_nvs([],               _,  _).
display_nvs([NameValue|Dict], Op, WO) :-
	display_nv0(NameValue, Op, WO),
	list(Dict, display_nv(Op, WO)),
	nl.

instantiated(Name = Value) :- '$VAR'(Name) \== Value.

sel_instantiated(NameValue) --> {instantiated(NameValue)}, !, [NameValue].
sel_instantiated(_) --> [].

write_goal2(Op, Goal0, d(_, _, ADict0), AtVars0) :-
	current_fact(printopts(_, D, A, V)),
	sort(AtVars0, AtVars1),
	( V == true ->
	    apply_dict(t(ADict0, Goal0, AtVars1), ADict0,
		t(ADict, Goal, AtVars)),
	    map(ADict, sel_instantiated, AInst, [])
	;
	    ADict = ADict0,
	    Goal = Goal0,
	    AtVars = AtVars1
	),
	( A == true, AtVars \== [] ->
	    WriteOpts = [max_depth(D), numbervars(true)]
	; WriteOpts = [max_depth(D), numbervars(true), portrayed(true)]
	),
	write_op(Op, Goal, WriteOpts),
	(V == true -> list(AInst, display_nv(Op, WriteOpts)) ;  true),
	(A == true -> print_attributes(AtVars, Op, WriteOpts) ; true).

write_op(0'p, Goal, WriteOpts) :- write_term(Goal, WriteOpts).
write_op(0'd, Goal, _) :- display(Goal).
write_op(0'w, Goal, _) :- writeq(Goal).

uninstantiated(_ = Value) :- var(Value).

show_variable_values(Dict0, Dict, VarKind, Op, WO) :-
	include(instantiated,   Dict,  DictInst),
	include(uninstantiated, Dict0, DictUninst),
	( DictInst == [] -> true
	; format(user,
		'         {Instantiated ~w-defined variables in scope:~n',
		[VarKind]),
	    display_nvs(DictInst, Op, WO),
	    format(user, '         }~n', [])
	),
	( DictUninst == [] -> true
	;
	    (DictInst == [] -> All = '(all) ' ; All = ''),
	    format(user,
		'         {Uninstantiated ~w~w-defined variables in scope:~n',
		[All, VarKind]),
	    display_nvs(DictUninst, Op, WO),
	    format(user, '         }~n', [])
	).

:- meta_predicate write_goal_v(?, ?, pred(2)).
write_goal_v(X0, d(UDict0, CDict0, _), GetAttributedVars) :-
	append(UDict0, CDict0, Dict0),
	GetAttributedVars(X0-Dict0, UnsortedAtVars),
	sort(UnsortedAtVars, AtVars0),
	apply_dict(
	    t(AtVars0, UDict0, CDict0),
	    Dict0,
	    t(AtVars, UDict, CDict)),
	current_fact(printopts(Op, D, _, _)),
	( AtVars = [_|_] ->
	    WO = [max_depth(D), numbervars(true)]
	; WO = [max_depth(D), numbervars(true), portrayed(true)]
	),
	show_variable_values(UDict0, UDict, user,     Op, WO),
	show_variable_values(CDict0, CDict, compiler, Op, WO),
	(AtVars = [_|_] -> print_attributes(AtVars, Op, WO) ; true).

:- meta_predicate write_goal_v_name(?, ?, pred(2)).
write_goal_v_name(SName, d(UDict, CDict, _), GetAttributedVars) :-
	append(UDict, CDict, Dict),
	atom_codes(Name, SName),
	( member(Name=Value0, Dict) ->
	    GetAttributedVars(Value0, AtVars0),
	    apply_dict(Value0-AtVars0, Dict, Value-AtVars),
	    current_fact(printopts(Op, D, _, _)),
	    ( AtVars = [_|_] ->
		WO = [max_depth(D), numbervars(true)]
	    ; WO = [max_depth(D), numbervars(true), portrayed(true)]
	    ),
	    display_var(Name, Value0, Value, Op, WO),
	    (AtVars = [_|_] -> print_attributes(AtVars, Op, WO) ; true)
	;
	    format(user, '{~w not defined here}~n', [Name])
	).

display_var(Name, Value0, Value, Op, WO) :-
	display(Name),
	( var(Value0) ->
	    display_list([' = ', Value0])
	; true
	),
	( '$VAR'(Name) == Value -> true
	; display(' = '),
	    write_op(Op, Value, WO)
	).

warn_if_udp(F, _, _) :- '$predicate_property'(F, _, _), !.
warn_if_udp(_, N, A) :-
	format(user_error, '{Warning: No definition for ~q}~n', [N/A]).

install_spypoint(F, N, A) :-
	'$spypoint'(F, on, on), !,
	format(user, '{There is already a spypoint on ~q}~n', [N/A]).
install_spypoint(F, N, A) :-
	'$spypoint'(F, off, on), !,
	format(user, '{Spypoint placed on ~q}~n', [N/A]).
install_spypoint(_, N, A) :-
	format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

remove_spypoint(F, N, A) :-
	'$spypoint'(F, off, off), !,
	format(user, '{There is no spypoint on ~q}~n', [N/A]).
remove_spypoint(F, N, A) :-
	'$spypoint'(F, on, off), !,
	format(user, '{Spypoint removed from ~q}~n', [N/A]).
remove_spypoint(_, N, A) :-
	ttynl,
	format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

spy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	install_spypoint(Pred, N, A).

nospy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	remove_spypoint(Pred, N, A).

:- pred nospyall/0 # "Remove all spy-points.".

nospyall :-
	spypoint(F),
	'$spypoint'(F, _, off),
	fail.
nospyall :-
	format(user, '{All spypoints removed}~n', []).

spypoint(X) :-
	'$current_predicate'(_, X),
	'$spypoint'(X, on, on).

all_spypoints :-
	spypoint(_),
	!,
	ttydisplay('Spypoints:'),
	list_spypoints.
all_spypoints :-
	ttydisplay('{There are no spypoints}'), ttynl.

list_spypoints :-
	spypoint(X),
	functor(X, N, A),
	ttynl, tab(user, 4), write(user, N/A),
	fail.
list_spypoints :-
	ttynl.

:- pred breakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
	: atm * sourcename * int * int * int * int

# "Set a @index{breakpoint} in file @var{Src} between lines
          @var{Ln0} and @var{Ln1} at the literal corresponding to the
          @var{Number}'th occurence of (predicate) name @var{Pred}.  The
          pair @var{Ln0}-@var{Ln1} uniquely identifies a program clause and
          must correspond to the
          start and end line numbers for the clause. The rest of the
          arguments provide enough information to be able to locate the
          exact literal that the @var{RealLine} line refers to. This is
          normally not issued by users but rather by the @apl{emacs} mode,
          which automatically computes the different argument after
          selecting a point in the source file.".

:- pred breakpoint(Pred, Src, Ln0, Ln1, Number) # "Breakpoint storage.".

:- data breakpoint/5.

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)), !,
	format(user,
	    '{There is already a breakpoint on literal ~a in line ~d}~n',
	    [Pred, RealLine]).

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	get_debugger_state(State),
	(arg(1, State, off) -> debug ; true),
	assertz_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
	format(user, '{Breakpoint placed on literal ~a in line ~d}~n',
	    [Pred, RealLine]).

:- pred nobreakall/0 # "Remove all breakpoints.".

nobreakall :-
	retractall_fact(breakpoint(_, _, _, _, _)),
	format(user, '{All breakpoints removed}~n', []).

:- pred nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
	: atm * sourcename * int * int * int * int
# "Remove a breakpoint in file @var{Src} between lines
          @var{Ln0} and @var{Ln1} at the @var{Number}'th occurence of
          (predicate) name @var{Pred} (see @pred{breakpt/6}). Also 
	  normally used from de @apl{emacs} mode.".

nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	retract_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)), !,
	format(user, '{Breakpoint removed from literal ~a in line ~d}~n',
	    [Pred, RealLine]).
nobreakpt(Pred, _, _, _, _, RealLine) :-
	format(user, '{No breakpoint on literal ~a in line ~d}~n',
	    [Pred, RealLine]).

:- pred list_breakpt/0 # "Prints out the location of all
	breakpoints. The location of the breakpoints is showed usual by
	referring to the source file, the lines between which the predicate
	can be found, the predicate name and the number of ocurrence of the
	predicate name of the literal.".

list_breakpt:-
	current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
	format(user, 'Breakpoint in file ~a ~d-~d on literal ~a-~d~n',
	    [Src, Ln0, Ln1, Pred, Number]),
	fail.
list_breakpt.

break_info(Pred, Src, _Ln0, Ln1, Number, ' B ') :-
	current_fact(breakpoint(Pred, Src, _, Ln1, Number)),
	!.
break_info(_Pred, _Src, _Ln0, _Ln1, _Number, '   ').

show_leash_info([]) :- !,
	format(user, '{No leashing}~n', []).
show_leash_info(Ps) :-
	format(user, '{Using leashing stopping at ~w ports}~n', [Ps]).

debugger_setting(Old, New) :-
	get_debugger_state(State),
	arg(1, State, Old),
	'$setarg'(1, State, New, true),
	adjust_debugger_state(State, New).

get_debugger_state(L) :-
	'$debugger_state'(L, L).

adjust_debugger_state(State, New) :-
	'$setarg'(2, State, New,     true),
	'$setarg'(3, State, 1000000, true),
	'$debugger_mode'.

:- pred debug/0 # "Switches the debugger on. The interpreter will
        stop at all ports of procedure boxes of spied predicates.".

debug :-
	debugger_setting(_, debug),
	what_is_on(debug).

call_hook2(answer(X), X).
call_hook2(no,        X) :- call_hook1(X).

call_hook1(X) :-
	( '$predicate_property'(X, _, _) ->
	    term_to_meta(X, G),
	    '$nodebug_call'(G)
	;
	    get_debugger_state(State),
	    adjust_debugger_state(State, trace),
	    functor(X, Name, Ar),
	    format(user_error, '{Warning: The predicate ~q is undefined}~n',
		[Name/Ar]),
	    fail
	).

a_length([],                0).
a_length([a(_, _, X, _)|_], X).

:- data debugdepth/1.

debugdepth(100000).

retry_hook(_,          P,  P, _).
retry_hook(Invocation, P0, P, A) :- retry_hook(Invocation, P0, P, A).

retry_hook_(X, B, D, [a(B, X, D, Dict)|A], A, Port, State, Dict) :-
	State = s(_, _, _, B0, A),
	a_length(A, D0),
	B is B0+1,
	D is D0+1,
	( current_fact(debugdepth(M)), D=<M -> true
	; adjust_debugger_state(State, trace),
	    warning('Interpreter maxdepth exceeded')
	),
	retry_hook(B, call, Port, '$$retry_hook').

port_info(block,   '  Block: ').
port_info(unblock, '  Unblock: ').
port_info(call,    '  Call: ').
port_info(exit,    '  Exit: ').
port_info(redo,    '  Redo: ').
port_info(fail,    '  Fail: ').
port_info(void,    '  ').

print_srcdbg_info(_,    _,   nil, nil, nil) :- !.
print_srcdbg_info(Pred, Src, Ln0, Ln1, Number) :-
	(
	    using_windows ->
% Emacs understand slashes instead of backslashes, even on
% Windows, and this saves problems with escaping
% backslashes
	    atom_codes(Src, SrcCodes),
	    cyg2win(SrcCodes, ActualSrcCodes, noswap),
	    atom_codes(ActualSrc, ActualSrcCodes)
	;
	    Src = ActualSrc
	),
	display_list(['         In ', ActualSrc, ' (', Ln0, -, Ln1, ') ',
		Pred, -, Number, '\n']).

spy_info([],       Goal, ' + ') --> {'$spypoint'(Goal, on, on)}, !.
spy_info([],       _,    '   ') --> [].
spy_info([I-X|Xs], _,    Goal) -->
	spy_info(Xs, X, Goal),
	[^, I].

lastof([],      X, X).
lastof([X0|Xs], _, X) :- lastof(Xs, X0, X).

do_retry_fail(B, State, Port) :-
	'$retry_cut'(B, Port),
	'$setarg'(2, State, trace, true), % retry implies creep!
	fail.

reset_debugger(State) :-
	'$debugger_state'(State, s(off, off, 1000000, 0, [])),
	'$debugger_mode'.

set_debugger(State) :-
	'$debugger_state'(_, State),
	'$debugger_mode'.

:- data leashed/1.

leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

:- pred nodebug/0 # "Switches the debugger off.  If there are any
        spy-points set then they will be kept but disabled.".

nodebug :- notrace.

:- pred trace/0 # "Start tracing, switching the debugger on if
        needed.  The interpreter will stop at all leashed ports of
        procedure boxes of predicates either belonging to debugged
        modules or called from clauses of debugged modules.  A message
        is printed at each stop point, expecting input from the user
        (write @tt{h} to see the available options).".

trace :-
	debugger_setting(_, trace),
	what_is_on(trace).

:- pred notrace/0 # "Equivalent to @pred{nodebug/0}.".

notrace :-
	debugger_setting(_, off),
	what_is_on(off).

:- doc(doinclude, multpredspec/1).

:- true prop multpredspec/1 + regtype.

multpredspec(Mod:Spec) :- atm(Mod), multpredspec(Spec).
multpredspec(Name/Low-High) :- atm(Name), int(Low), int(High).
multpredspec(Name/(Low-High)) :- atm(Name), int(Low), int(High).
multpredspec(Name/Arity) :- atm(Name), int(Arity).
multpredspec(Name) :- atm(Name).

functor_spec(Mod:Spec, Name, Low, High, Mod) :-
	functor_spec(Spec, Name, Low, High, _).
functor_spec(Name/Low-High, Name, Low, High, _) :-
	atom(Name),
	integer(Low), integer(High), !.
functor_spec(Name/(Low-High), Name, Low, High, _) :-
	atom(Name),
	integer(Low), integer(High), !.
functor_spec(Name/Arity, Name, Arity, Arity, _) :-
	atom(Name),
	integer(Arity), !.
functor_spec(Name, Name, 0, 255, _) :- % 255 is max. arity
	atom(Name).

