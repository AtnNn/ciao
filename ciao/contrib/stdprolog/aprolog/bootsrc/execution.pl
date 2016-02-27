:- module(execution, [repeat/0, halt/0, halt/1,
		      catch/3, throw/1,
		      call/1, once/1, (\+)/1,
		      current_prolog_flag/2, set_prolog_flag/2]).

:- use_module(err, [err_check/2]).
:- use_module(lists, [member/2, memberchk/2]).
:- use_module(io, [error_stream/1]).
:- use_module(misc, [format/3]).

% ----------------
repeat.
repeat:-
	repeat.

% ----------------
halt :-
    '$halt'(0).

halt(N) :-
	integer(N), !,
	'$halt'(N).
halt(N) :-
	err_check(halt(N),
	          [inst(N),
		   integer(N)]).

% ----------------
catch(Goal, Pattern, Handler) :-
	catch_some(Goal, Pattern, Handler, ['$abort', '$retry']).

catch_some(Goal, _, _, _) :-
	catch_try(Goal).
catch_some(_, Pattern, Handler, Not) :-
	retract('$exception_term'(Exception)),
	do_exception(Pattern, Exception, Handler, Not).

catch_try(Goal) :-
	'$choice'(T),
	'$get_global'('$exception_choice', PT),
	'$set_global'('$exception_choice', T),
	interpret(Goal, T),
	'$set_global'('$exception_choice', PT).

do_exception(Exception, Exception, Handler, Not) :-
	\+ member(Exception, Not),
	!,
	call(Handler).
do_exception(_, Exception, _, _) :-
	throw(Exception).

throw(Exception) :-
	nonvar(Exception), !,
	asserta('$exception_term'(Exception)),
	'$get_global'('$exception_choice', T),
	'$cut'(T),
	fail.
throw(Exception) :-
	err_check(throw(Exception), [inst(Exception)]).
		   

% ----------------
ifthenelse(X, Y, _, T):-
	call_i(X), !,
	interpret(Y, T).
ifthenelse(_, _, Z, T):-
	interpret(Z, T).

interpret(X, _):-
	'$var_index'(X, _),  % don't want non-hidden call in interpret
	X.
interpret((X->Y), T):-
	!,
	ifthenelse(X, Y, fail, T).
interpret(((X->Y);Z), T):-
	!, 
	ifthenelse(X, Y, Z, T).
interpret((X;_), T):-
	interpret(X, T).
interpret((_;X), T):-
	!,
	interpret(X, T).
interpret((X,Y), T):-
	!,
	interpret(X, T),
	interpret(Y, T).
interpret(!, T):-
	!,
	'$cut'(T).
interpret(X, _):-
	X.

% ----------------
call_i(X) :-
	'$choice'(T),
	interpret(X, T).

% ----------------

toplevel_call(X) :-
	nonvar(X),
	convert_to_goal(X, Goal), !,
	'debug:reset',
	'$set_global'('$can_debug', true),
	'$choice'(T),
	interpret(Goal, T),
	'$set_global'('$can_debug', false).
toplevel_call(X) :-
	err_check(call(X),
		   [inst(X),
		    conv_to_goal(X)]).

% ----------------
call(X) :-
	nonvar(X),
	convert_to_goal(X, G), !, 
	'$choice'(T),
	interpret(G, T).
call(X) :-
	err_check(call(X),
		   [inst(X),
		    conv_to_goal(X)]).

convert_to_goal(X, call(X)) :-
	var(X), !.
convert_to_goal((X1,X2), (G1,G2)) :- !,
	convert_to_goal(X1, G1),
	convert_to_goal(X2, G2).
convert_to_goal((X1;X2), (G1;G2)) :- !,
	convert_to_goal(X1, G1),
	convert_to_goal(X2, G2).
convert_to_goal((X1->X2), (G1->G2)) :- !,
	convert_to_goal(X1, G1),
	convert_to_goal(X2, G2).
convert_to_goal(X, X) :-
	callable(X).

% ----------------
once(X) :-
        call(X), !.

% ----------------
\+ X :-
	call(X), !, fail.
\+ _.

% ----------------
not(X) :-
	X, !, fail.
not(_).

% ----------------
call_cleanup_det(Call, Cleanup) :-
	catch_some(Call, E, (cleanup(Cleanup), throw(E)), ['$retry']), !,
	cleanup(Cleanup), !.
call_cleanup_det(_, Cleanup) :-
	cleanup(Cleanup), !, fail.

cleanup(Cleanup) :-
	call(Cleanup), !.
cleanup(Cleanup) :-
	throw(error(internal_error(Cleanup), call_cleanup_det/2)).

% ----------------
call_dynamic(Goal):-
	nodebug(clause(Goal, Body)),
	'$choice'(T),
	interpret(Body, T).

% ----------------
call_error(Goal) :-
	nodebug(call_error_do(Goal)).

call_error_do(Goal) :-
	callable(Goal), !,
	current_prolog_flag(unknown, Type),
	call_error_type(Type, Goal).
call_error_do(Goal) :-
	throw(error(internal_error(callable, Goal), execution)).

call_error_type(fail, _) :- !,
	fail.
call_error_type(warning, Goal) :- !,
        functor(Goal, Name, Arity),
	format(user_output, '\n{Warning: the procedure ~q is undefined}',
	       [Name/Arity]),
	fail.
call_error_type(error, Goal) :- !,
        functor(Goal, Name, Arity),
        throw(error(existence_error(procedure, Name/Arity), execute)).

% ----------------
nodebug(Call) :-
	'$get_global'('$can_debug', false), !,
	Call.
nodebug(Call) :-
	'$set_global'('$can_debug', false),
	Call,
	'$set_global'('$can_debug', true).

% ----------------
abort :-
	throw('$abort').

% ----------------
write_exception(S, error(ISO,Detail)) :- !,
	format(S, '{ERROR: ~q in ~q}', [ISO,Detail]).
write_exception(S, '$abort') :- !,
	write(S, '{Execution aborted}').
write_exception(S, Exception) :-
	format(S, '{ERROR: uncaught exception: ~q}', [Exception]).

% ----------------
current_prolog_flag(Flag, Value) :-
	var(Flag), !,
	clause('$prolog_flag'(Flag, Value, _), _).
current_prolog_flag(Flag, Value) :-
	atom(Flag), 
	clause('$prolog_flag'(Flag, Value0, _), _), !,
	Value = Value0.
current_prolog_flag(Flag, Value) :-
	err_check(current_prolog_flag(Flag, Value),
		   [atom(Flag),
		    prolog_flag(Flag)]).

% ----------------
set_prolog_flag(Flag, Value) :-
	nonvar(Flag),
	nonvar(Value),
	atom(Flag),
	clause('$prolog_flag'(Flag, _, true), _),
	flag_value_ok(Flag, Value), !,
	retract('$prolog_flag'(Flag, _, _)),
	asserta('$prolog_flag'(Flag, Value, true)),
	flag_set(Flag, Value).
set_prolog_flag(Flag, Value) :-
	err_check(set_prolog_flag(Flag, Value),
		   [inst(Flag),
		    inst(Value),
		    atom(Flag),
		    prolog_flag(Flag),
		    prolog_flag_modify(Flag),
		    prolog_flag_value(Flag, Value)
		   ]).

flag_ok(Flag) :- clause('$prolog_flag'(Flag, _, _), _).
flag_modifiable(Flag) :- clause('$prolog_flag'(Flag, _, true), _).

flag_value_ok(char_conversion, X) :- memberchk(X, [off, on]).
flag_value_ok(debug, X) :-           memberchk(X, [off, on]).
flag_value_ok(unknown, X) :-         memberchk(X, [error, fail, warning]).
flag_value_ok(double_quotes, X) :-   memberchk(X, [codes, chars, atom]).

flag_set(char_conversion, _) :- !,
	'term_io:check_char_conv_state'.
flag_set(_, _).

% ----------------	
init_prolog_flags :-
	assertz('$prolog_flag'(bounded, true, false)),
	Min is 1 << 30,
	Max is \Min,
	assertz('$prolog_flag'(max_integer, Max, false)),
	assertz('$prolog_flag'(min_integer, Min, false)),
	assertz('$prolog_flag'(integer_rounding_function, toward_zero, false)),
	assertz('$prolog_flag'(max_arity, 255, false)),
	%%%% pts %%%%
	(    '$get_bb'('$version', Version) -> assertz('$prolog_flag'(version, Version, false))
	;    true
	),
	assertz('$prolog_flag'(char_conversion, on, true)),
	assertz('$prolog_flag'(debug, off, true)),
	assertz('$prolog_flag'(unknown, error, true)),
	assertz('$prolog_flag'(double_quotes, codes, true)).

% ----------------
init_builtins :-
	'$set_global'('$exception_choice', 0),
	'$set_global'('$can_debug', false),
	init_prolog_flags,
        'term_io:init_ops',
	'term_io:init_charconv',
        'io:init_streams'.
        
