:- module(debugger, [
        initialize_debugger/0, adjust_debugger/0,
        debug_module/1, nodebug_module/1,
        debug/0, nodebug/0, trace/0, notrace/0, spy/1, nospy/1,
 	nospyall/0, debugging/0, leash/1, maxdepth/1, call_in_module/2,
        current_debugged/1, debugger_setting/2,
        reset_debugger/1, set_debugger/1, get_debugger_state/1,
        retry_hook/4,
        debug_trace/1,
        do_interrupt_command/1],
	[dcg,assertions]).

:- use_module(engine(internals)).
:- use_module(library(format)).
:- use_module(library(ttyout)).
:- use_module(library(read), [read/2]).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(library(prolog_sys), [current_predicate/2]).

:- comment(title, "Predicates controlling the interactive debugger").

:- comment(module, "This library implements predicates which are
   normally used in the interactive top-level shell to debug
   programs.").

:- comment(hide, initialize_debugger/0).
:- comment(hide, adjust_debugger/0).
:- comment(hide, current_debugged/1).
:- comment(hide, debugger_setting/2).
:- comment(hide, reset_debugger/1).
:- comment(hide, set_debugger/1).
:- comment(hide, get_debugger_state/1).
:- comment(hide, retry_hook/4).
:- comment(hide, debug_trace/1).
:- comment(hide, do_interrupt_command/1).

:- comment(author,"A. Ciepielewski, M. Carlsson, T. Chikayama,
   K. Shen, D. Cabeza").

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

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*3+104,1999/11/17,22:05*53+'MET'), "Fixed a bug when
   calling debugging/0 by inputing '=' at the trace prompt (Daniel
   Cabeza Gras)").

:- comment(version(0*6+3,1998/07/20,18:51*26+'MET DST'), "Added
   debug_module/1 and nodebug_module/1 to replace debug_modules/1
   (Daniel Cabeza Gras)").

:- comment(version(0*6+1,1998/07/20,18:18*07+'MET DST'), "When tracing,
   all calls issued in debugged predicates are printed (Daniel
   Cabeza Gras)").

:- comment(version(0*4+11,1998/3/12), "Integrated debugger in toplevel
   (Daniel Cabeza Gras)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

:- comment(version(0*0+0,1998/2/23), "Added inclusion of dcg syntax
   lib. (Manuel Hermenegildo)").

% :- multifile define_flag/3.
% 
% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

initialize_debugger :- reset_debugger(_).

reset_debugger(State) :-
	'$debugger_state'(State, s(off,off,1000000,0,[])),
	'$debugger_mode'.

set_debugger(State) :-
	'$debugger_state'(_, State),
	'$debugger_mode'.

get_debugger_state(L) :-
	'$debugger_state'(L, L).

:- data debug_mod/2.

:- true pred debug_module(Module) : atm(Module)
        # "The debugger will take into acount module @var{Module}
          (assuming it is is loaded in consult mode).  When issuing this
          command at the toplevel shell, the compiler is instructed also
          to set to @em{consult} the loading mode of files defining that
          module.".

debug_module(M) :-
        atom(M),
        ( current_fact(debug_mod(M,_)), !
        ; atom_concat(M, ':', Mc),
          assertz_fact(debug_mod(M, Mc))
        ),
        what_is_debugged.
debug_module(M) :-
        format(user_error, '{Bad module ~q - must be an atom}~n', [M]).

:- true pred nodebug_module(Module) : atm(Module)
        # "The debugger will not take into acount module @var{Module}.
          When issuing this command at the toplevel shell, the compiler is
          instructed also to set to @em{compile} the loding mode of files
          defining that module.".

nodebug_module(M) :- % If M is a var, nodebug for all
        retractall_fact(debug_mod(M,_)),
        what_is_debugged.

current_debugged(Ms) :- findall(M, current_fact(debug_mod(M,_)), Ms).

:- data printdepth/1.

printdepth(10).

:- data debugdepth/1.

debugdepth(100000).

:- true pred maxdepth(MaxDepth) : int
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

:- true pred debug/0 # "Switches the debugger on. The interpreter will
        stop at all ports of procedure boxes of spied predicates.".

debug :-
	debugger_setting(_, debug),
	what_is_on(debug).

:- true pred nodebug/0 # "Switches the debugger off.  If there are any
        spy-points set then they will be kept but disabled.".

nodebug :- notrace.

:- true pred trace/0 # "Start tracing, switching the debugger on if
        needed.  The interpreter will stop at all leashed ports of
        procedure boxes of predicates either belonging to debugged
        modules or called from clauses of debugged modules.  A message
        is printed at each stop point, expecting input from the user
        (write @tt{h} to see the available options).".

trace :-
	debugger_setting(_, trace),
	what_is_on(trace).

:- true pred notrace/0 # "Equivalent to @pred{nodebug/0}.".

notrace :-
	debugger_setting(_, off),
	what_is_on(off).

:- true pred spy(PredSpec) : sequence(multpredspec)
        # "Set spy-points on predicates belonging to debugged modules
          which match @var{PredSpec}, switching the debugger on if
          needed. This predicate is defined as a prefix operator by the
          toplevel.".

spy(Preds) :-
        get_debugger_state(State),
	( arg(1, State, off) -> debug ; true ),
        parse_functor_spec(Preds, X, spy1(X)).

:- true pred nospy(PredSpec) : sequence(multpredspec)
        # "Remove spy-points on predicates belonging to debugged modules
          which match @var{PredSpec}. This predicate is defined as a prefix
          operator by the toplevel.".

nospy(Preds) :-
        parse_functor_spec(Preds, X, nospy1(X)).

spy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	install_spypoint(Pred, N, A).

nospy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	remove_spypoint(Pred, N, A).

:- true pred nospyall/0 # "Remove all spy-points.".

nospyall :-
	spypoint(F),
	'$spypoint'(F, _, off),
	fail.
nospyall :-
	format(user, '{All spypoints removed}~n', []).

debugger_setting(Old, New) :-
	get_debugger_state(State),
	arg(1, State, Old),
	'$setarg'(1, State, New, true),
	adjust_debugger_state(State, New).

:- data leashed/1.

leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

:- true pred leash(Ports) : list(atm)
        # "Leash on ports @var{Ports}, some of @tt{call}, @tt{exit},
          @tt{redo}, @tt{fail}.".

leash(L) :-
	nonvar(L),
	leash1(L),
	!.
leash(L) :-
        format(user_error, '{Bad leash specification ~q}~n', [L]).

leash1(half)  :- !, leash1([call,redo]).
leash1(full)  :- !, leash1([call,exit,redo,fail]).
leash1(loose) :- !, leash1([call]).
leash1(none)  :- !, leash1([]).
leash1(tight) :- !, leash1([call,redo,fail]).
leash1(L) :-
        list(L),
        retractall_fact(leashed(_)), leashlist(L), what_is_leashed.

leashlist([]).
leashlist([Port|L]) :-
        assertz_fact(leashed(Port)),
        leashlist(L).

:- true pred debugging/0 # "Display debugger state.".

debugging :-
	get_debugger_state(State),
	arg(1, State, G),
	what_is_on(G),
        what_is_debugged,
	what_is_leashed,
	what_maxdepth,
	all_spypoints,
	ttynl.
	
%------------------------ meta-interpreters ------------------------------

% called from interpreter.pl

debug_trace(X) :-
        in_debug_module(X), !,
        debug_trace2(X).
debug_trace(X) :-
        get_debugger_state(S),
        arg(5, S, [a(_,Ancestor,_)|_]),
        in_debug_module(Ancestor), !,
        debug_trace2(X).
debug_trace(X) :-
        term_to_meta(X, G), '$nodebug_call'(G).

in_debug_module(G) :-
        functor(G, F, _),
        current_fact(debug_mod(_,Mc)),
        atom_concat(Mc, _, F).

debug_trace2(X) :-
% 	blocked_goal(X), !,
% 	(   get_debugger_state(State),
% 	    State = s(_,trace,Level,B,_),
% 	    B < Level ->
% 	    write_goal(0'p, X, [], -, -, block),
% 	    ttynl
% 	;   true
% 	).
% debug_trace2(X) :- byrd_box(X).
% 
% blocked_goal(X) :-
% 	arg(1, X, X1),
% 	var(X1),
% 	'$predicate_property'(X, _, P),
% 	4 is P/\4,				% wait, xref nondet.c
%         term_to_meta(X, G),
% 	'$nodebug_call'(G).
% 
% byrd_box(X) :-
	get_debugger_state(State),
	retry_hook_(X, B, D, NA, OA, Port, State),
	'$setarg'(4, State, B, on),
	'$setarg'(5, State, NA, on),
	(   Port=call,
	    call_hook(X, B, D, State)
	;   fail_hook(X, B, D, State), !, fail
	),
	(   exit_hook(X, B, D, State)
	;   redo_hook(X, B, D, State), fail
	),
	'$setarg'(5, State, OA, on).



call_hook(X, B, _, State) :-
	arg(3, State, Level),
	B>Level, !,
	call_hook1(X).
call_hook(X, B, D, State) :-
	debug_port(X, B, D, call, State, Msg),
	call_hook2(Msg, X).

call_hook1(X) :-
	(   '$predicate_property'(X, _, _) ->
            term_to_meta(X, G),
	    '$nodebug_call'(G)
        ;   get_debugger_state(State),
	    adjust_debugger_state(State, trace),
	    functor(X, Name, Ar),
	    format(user_error, '{Warning: The predicate ~q is undefined}~n',
	                       [Name/Ar]),
            fail
        ).

call_hook2(answer(X), X).
call_hook2(no, X) :- call_hook1(X).

exit_hook(_, B, _, State) :- arg(3, State, Level), B>Level, !.
exit_hook(X, B, D, State) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, exit, State, _).

redo_hook(_, B, _, State) :- arg(3, State, Level), B>Level, !.
redo_hook(X, B, D, State) :- debug_port(X, B, D, redo, State, _).

fail_hook(_, B, _, State) :- arg(3, State, Level), B>Level, !.
fail_hook(X, B, D, State) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, fail, State, _).

retry_hook(_, P, P, _).
retry_hook(Invocation, P0, P, A) :- retry_hook(Invocation, P0, P, A).

retry_hook_(X, B, D, [a(B,X,D)|A], A, Port, State) :-
	State = s(_,_,_,B0,A),
	a_length(A, D0),
	B is B0+1,
	D is D0+1,
	(   current_fact(debugdepth(M)), D=<M -> true
	;   adjust_debugger_state(State, trace),
	    format(user_error,
	           '{Warning: Interpreter maxdepth exceeded}~n', [])
	),
	retry_hook(B, call, Port, '$$retry_hook').

a_length([], 0).
a_length([a(_,_,X)|_], X).

debug_port(X, B, D, Port, State, Msg) :-
	'$spypoint'(X, on, on), !,
	prompt_command(0'p, X, [], B, D, Port, State, Msg).
debug_port(X, B, D, Port, State, Msg) :-
	arg(2, State, trace),
	current_fact(leashed(Port)), !,
	prompt_command(0'p, X, [], B, D, Port, State, Msg).
debug_port(X, B, D, Port, State, no) :-
	arg(2, State, trace), !,
	write_goal(0'p, X, [], B, D, Port),
	ttynl.
debug_port(_, _, _, _, _, no).

prompt_command(T, X, Xs, B, D, Port, State, Msg) :-
	write_goal(T, X, Xs, B, D, Port),
	get_command(C),
	do_trace_command(C, X, Xs, B, D, Port, State, Msg).

do_trace_command(0'a, _, _, _, _, _, _, _) :- !, % a(bort)
	abort.
do_trace_command(0'c, _, _, _, _, _, State, no) :- !, % c(reep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'
	        , _, _, _, _, _, State, no) :- !, % CR (creep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'd, X, Xs, B, D, Port, State, Msg) :- !, % d(isplay)
	prompt_command(0'd, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'g, X, Xs, B, D, Port, State, Msg) :- !, % g(ancestors)
	arg(5, State, CA),
	show_ancestors(CA, -1),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command([0'g,Arg], X, Xs, B, D, Port, State, Msg):- !, % g(ancestors) arg
	arg(5, State, CA),
	show_ancestors(CA, Arg),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'l, _, _, _, _, _, State, no) :- !, % l(eap)
	'$setarg'(2, State, debug, true),
	'$debugger_mode'.
do_trace_command(0'n, _, _, _, _, _, State, no) :- !, % n(odebug)
	'$setarg'(3, State, 0, true).
do_trace_command(0'p, X, Xs, B, D, Port, State, Msg) :- !, % p(rint)
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'r, _, _, _, _, _, State, no) :- !, % r(etry)
	arg(5, State, [a(B,_,_)|_]),
	do_retry_fail(B, State, call).
do_trace_command([0'r,B], _, _, _, _, _, State, no) :- !, % r(etry) arg
	do_retry_fail(B, State, call).
do_trace_command(0'f, _, _, _, _, _, State, no) :- !, %f(ail)
	arg(5, State, [a(B,_,_)|_]),
	do_retry_fail(B, State, fail).
do_trace_command([0'f,B], _, _, _, _, _, State, no) :- !, % f(ail) arg
	do_retry_fail(B, State, fail).
do_trace_command(0's, _, _, B, _, Port, State, no) :- % s(kip)
	set_skip(Port, B, State), !.
do_trace_command(0'w, X, Xs, B, D, Port, State, Msg) :- !, % w(rite)
	prompt_command(0'w, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'+, X, Xs, B, D, Port, State, Msg) :- !, % +(spy this)
	lastof(Xs, _-X, _-Goal),
	spy1(Goal),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'-, X, Xs, B, D, Port, State, Msg) :- !, % -(nospy this)
	lastof(Xs, _-X, _-Goal),
	nospy1(Goal),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'=, X, Xs, B, D, Port, State, Msg) :- !, % =(debugging)
        reset_debugger(_),
	debugging,
        set_debugger(State),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
% do_trace_command(0'b, X, Xs, B, D, Port, State, Msg) :- !, % b(reak)
% 	break,
% 	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'@, X, Xs, B, D, Port, State, Msg) :- !, %@ (command)
	do_once_command('| ?- '),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'u, _, _, _, _, call, _, answer(X1)) :- !, %u (unify)
	'$prompt'(Old, '|: '),
	read(user, X1),
	'$prompt'(_, Old).
do_trace_command(0'<, X, Xs, B, D, Port, State, Msg) :- !, %< (reset printdepth)
	retractall_fact(printdepth(_)),
        assertz_fact(printdepth(10)),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command([0'<,I], X, Xs, B, D, Port, State, Msg) :- !, %< arg (set printdepth)
	retractall_fact(printdepth(_)),
        assertz_fact(printdepth(I)),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'^, X, Xs, B, D, Port, State, Msg) :- !, %^ (reset subterm)
	lastof(Xs, _-X, _-Goal),
	prompt_command(0'p, Goal, [], B, D, Port, State, Msg).
do_trace_command([0'^,0], _, [_-X|Xs], B, D, Port, State, Msg) :- !, %^ 0 (up subterm)
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command([0'^,I], X, Xs, B, D, Port, State, Msg) :- %^ arg (set subterm)
	arg(I, X, Ith), !,
	prompt_command(0'p, Ith, [I-X|Xs], B, D, Port, State, Msg).
do_trace_command(0'?, X, Xs, B, D, Port, State, Msg) :- !, % ?(help)
	debugging_options,
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'h, X, Xs, B, D, Port, State, Msg) :- !, % h(elp)
	debugging_options,
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(_, X, Xs, B, D, Port, State, Msg) :- % all others
	format(user, '{Option not applicable at this port}~n', []),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).

lastof([], X, X).
lastof([X0|Xs], _, X) :- lastof(Xs, X0, X).

do_retry_fail(B, State, Port) :-
	'$retry_cut'(B, Port),
	'$setarg'(2, State, trace, true),	% retry implies creep!
	fail.

do_once_command(Prompt) :-
	'$prompt'(OldPrompt, Prompt),
	read(user, Command),
	reset_debugger(State),
	'$prompt'(_, '|: '),
	( call(Command) -> Y=yes
        ; Y=no
        ),
	'$prompt'(_, OldPrompt),
	set_debugger(State),
	Y=yes, !.
do_once_command(_) :-
        format(user_error, '{Warning: goal failed}~n', []).

%--------------------------options---------------------------------------

debugging_options :-
	ttydisplay('Debugging options:'), ttynl,
	ttydisplay('   <cr>   creep            c      creep'), ttynl,
	ttydisplay('    l     leap             s      skip'), ttynl,
	ttydisplay('    r     retry            r <i>  retry i'), ttynl,
	ttydisplay('    f     fail             f <i>  fail i'), ttynl,
	ttydisplay('    d     display          p      print'), ttynl,
	ttydisplay('    w     write'), ttynl,
	ttydisplay('    g     ancestors        g <n>  ancestors n'), ttynl,
	ttydisplay('    n     nodebug          =      debugging'), ttynl,
	ttydisplay('    +     spy this         -      nospy this'), ttynl,
	ttydisplay('    a     abort'), ttynl,
	ttydisplay('    @     command          u      unify'), ttynl,
	ttydisplay('    <     reset printdepth < <n>  set printdepth'), ttynl,
	ttydisplay('    ^     reset subterm    ^ <n>  set subterm'), ttynl,
	ttydisplay('    ?     help             h      help'), ttynl,
	ttynl.

%-------------------------facilities-------------------------------------

adjust_debugger :-
        get_debugger_state(State),
	arg(1, State, G),
	adjust_debugger_state(State, G),
        '$empty_gcdef_bin', % Really get rid of abolished predicates
        pre_prompt(G).

pre_prompt(off) :- !.
pre_prompt(X) :-
	format(user, '{~w}~n', [X]).

adjust_debugger_state(State, New) :-
	'$setarg'(2, State, New, true),
	'$setarg'(3, State, 1000000, true),
	'$debugger_mode'.

all_spypoints :-
	spypoint(_), !,
	ttydisplay('Spypoints:'), list_spypoints.
all_spypoints :-
	ttydisplay('{There are no spypoints}'), ttynl.

list_spypoints :-
	spypoint(X),
	functor(X, N, A),
	ttynl, tab(user, 4), write(user, N/A),
	fail.
list_spypoints :-
	ttynl.

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

spypoint(X) :-
	current_predicate(_, X),
	'$spypoint'(X, on, on).

write_goal(T, X, Xs, B, D, Port) :-
	port_info(Port, Pport),
	spy_info(Xs, X, Mark, S, []),
        current_output(CO),
        set_output(user),
        display_list([Mark, B, '  ', D, Pport | S]),
	write_goal2(T, X),
        set_output(CO).

spy_info([], Goal, ' + ') --> {'$spypoint'(Goal, on, on)}, !.
spy_info([], _, '   ') --> [].
spy_info([I-X|Xs], _, Goal) --> 
	spy_info(Xs, X, Goal),
	[^, I].

port_info(block, '  Block: ').
port_info(unblock, '  Unblock: ').
port_info(call, '  Call: ').
port_info(exit, '  Exit: ').
port_info(redo, '  Redo: ').
port_info(fail, '  Fail: ').
port_info(void, '  ').

write_goal2(0'd, Goal) :- display(Goal).	% display
write_goal2(0'p, Goal) :-			% print
	current_fact(printdepth(D)),
        get_attributed_vars(Goal, AtVars, []),
        (
            AtVars = [_|_] ->
            sort(AtVars, SortedAtVars),
            write_canonical(Goal),              % Skip user's portray_attribute
            print_attributes(SortedAtVars, D)
        ;
            write_term(Goal, [portrayed(true),max_depth(D)])
        ).
write_goal2(0'w, Goal) :- writeq(Goal).	% write

get_attributed_vars(X, At, At):- atomic(X), !.
get_attributed_vars(X, [attach_attribute(X,AtX)|At], At):- 
        var(X), 
        get_attribute(X, AtX), !.
get_attributed_vars(X, At, At):- var(X), !.     % No attributes
get_attributed_vars([X|Xs], At0, At2):- !,
        get_attributed_vars(X, At0, At1),
        get_attributed_vars(Xs, At1, At2).
get_attributed_vars(X, At0, At1):-
        functor(X, _, Ar),
        get_attributed_vars_args(Ar, X, At0, At1).

get_attributed_vars_args(0, _, At, At):- !.
get_attributed_vars_args(N, X, At0, At2):- 
        N > 0,
        arg(N, X, A),
        get_attributed_vars(A, At0, At1),
        N1 is N - 1,
        get_attributed_vars_args(N1, X, At1, At2).

print_attributes([], _D).
print_attributes([A|As], D):-
        nl,
        tab(10),    % 10 blanks
        display('['),
        write_canonical(A),
        display(']'),
        print_attributes(As, D).

get_command(Command) :-
	ttydisplay(' ? '),
	ttyflush,
	ttyget(C1),
	get_rest_command(C1, Command).

get_rest_command(0'
	         , 0'
	         ) :- !.
get_rest_command(C1, Command) :-
	ttyget(C2),
	get_arg(C2, C1, Command).

get_arg(0'
       , C, C):- !.
get_arg(C2, C1, [C1,Arg]) :-
	C2 >= 0'0, C2 =< 0'9, !,
	trd_digits(C2, 0, Arg).
get_arg(_, C1, C):-
	ttyget(C2),
	get_arg(C2, C1, C).

trd_digits(Ch, SoFar, I) :-
	Ch >= 0'0, Ch =< 0'9, !,
	Next is SoFar*10 + Ch - 0'0,
	ttyget(Ch1),
	trd_digits(Ch1, Next, I).
trd_digits(0'
	  , I, I) :- !.
trd_digits(_, I, J) :-
	ttyget(Ch),
	trd_digits(Ch, I, J).

show_ancestors([_], _) :- !,
	ttynl, ttydisplay('No ancestors.'), ttynl.
show_ancestors([_|CA], N) :-
	ttynl, ttydisplay('Ancestors:'), ttynl,
	list_ancestors(CA, N).

list_ancestors([], _) :- !.
list_ancestors(_, 0) :- !.
list_ancestors([a(B,X,D)|As], N0) :-
	N is N0-1,
	list_ancestors(As, N),
	write_goal(0'p, X, [], B, D, void),
	ttynl.

mode_message(debug,
	      '{The debugger will first leap -- showing spypoints (debug)}').
mode_message(trace,
	      '{The debugger will first creep -- showing everything (trace)}').
mode_message(off,
	      '{The debugger is switched off}').

what_is_on(Mode) :-
	mode_message(Mode, Msg),
	ttydisplay(Msg),
	ttynl.

what_is_debugged :-
        current_debugged(Ms),
        ( Ms = [] ->
          format(user, '{No module is selected for debugging}~n',[])
        ; format(user, '{Modules selected for debugging: ~w}~n',[Ms])
        ).

what_is_leashed :-
	is_leashed([call,exit,redo,fail], L),
	show_leash_info(L).

is_leashed([], []).
is_leashed([X|Xs], [X|Ys]) :- current_fact(leashed(X)), !, is_leashed(Xs, Ys).
is_leashed([_|Xs], Ys) :- is_leashed(Xs, Ys).

show_leash_info([]) :- !,
	format(user, '{No leashing}~n', []).
show_leash_info(Ps) :-
	format(user, '{Using leashing stopping at ~w ports}~n', [Ps]).

what_maxdepth :-
	current_fact(debugdepth(M)),
	format(user, '{Interpreter maxdepth is ~w}~n', [M]).


set_skip(call, To, State) :- '$setarg'(3, State, To, true).
set_skip(redo, To, State) :- '$setarg'(3, State, To, true).

:- meta_predicate parse_functor_spec(?,?,goal).

parse_functor_spec(V, _, _) :-
        var(V), !,
        format(user_error, '{A variable is a bad predicate indicator}~n', []).
parse_functor_spec((S,Ss), GoalArg, Goal) :-
        parse_functor_spec(S, GoalArg, Goal),
        parse_functor_spec(Ss, GoalArg, Goal).
parse_functor_spec(S, GoalArg, Goal) :-
	Flag=f(0),
	(   functor_spec(S, Name, Low, High, M),
            current_fact(debug_mod(M,Mc)),
            atom_concat(Mc, Name, PredName),
	    current_predicate(PredName, GoalArg),
	    functor(GoalArg, _, N),
	    N >= Low, N =< High,
	    '$setarg'(1, Flag, 1, true),
	    '$nodebug_call'(Goal),
	    fail
	;   Flag=f(0),
	    format(user_error,
               "{Bad predicate indicator or predicate undefined in modules currently debugged:~n ~w}~n", [S]),
            fail
	;   true
	).

:- comment(doinclude, multpredspec/1).

:- true prop multpredspec/1.

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
functor_spec(Name, Name, 0, 255, _) :-		% 255 is max. arity
	atom(Name).


do_interrupt_command(0'@) :- !,			% @ (command)
	ttyskipeol, do_once_command('| ?- '),
        do_interrupt_command(0'
                            ).
do_interrupt_command(0'a) :- !,			% a(bort)
	ttyskipeol, abort.
% do_interrupt_command(0'b) :- !,			% b(reak)
% 	ttyskipeol, break.
do_interrupt_command(0'c) :- !,			% c(ontinue)
	ttyskipeol.
do_interrupt_command(0'd) :- !,			% d(ebug)
	ttyskipeol, debug.
do_interrupt_command(0'e) :- !,			% e(xit)
	ttyskipeol, halt.
do_interrupt_command(0't) :- !,			% t(race)
	ttyskipeol, trace.
do_interrupt_command(0'
	            ) :- !,			% cr
	format(user, '~nCIAO interruption (h for help)? ', []),
	ttyflush,
	ttyget(C),
	do_interrupt_command(C).
do_interrupt_command(_) :-			% h(elp) or other
	ttyskipeol,
	interrupt_options,
	do_interrupt_command(0'
	                    ).

interrupt_options :-
	ttynl,
	ttydisplay('CIAO interrupt options:'), ttynl,
	ttydisplay('    a        abort           - cause abort'), ttynl,
%	ttydisplay('    b        break           - cause break'), ttynl,
	ttydisplay('    c        continue        - do nothing'), ttynl,
	ttydisplay('    d        debug           - start debugging'), ttynl,
	ttydisplay('    t        trace           - start tracing'), ttynl,
	ttydisplay('    e        exit            - cause exit'), ttynl,
	ttydisplay('    @        command         - execute a command'), ttynl,
	ttydisplay('    h        help            - get this list'), ttynl.

% :- meta_predicate call_in_module(?, fact).

:- true pred call_in_module(Module, Predicate) : atm * callable
        # "Calls predicate @var{Predicate} belonging to module
          @var{Module}, even if that module does not export the
          predicate.".

call_in_module(Module, Goal) :-
        module_concat(Module, Goal, MGoal),
        '$meta_call'(MGoal).

%---------------------------------------------------------------------------
