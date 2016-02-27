:- module(debug, [trace/0, notrace/0]).

:- use_module(database, [retractall/1]).

% ----------------
trace :-
	retractall('$trmode'(_)),
	asserta('$trmode'(creep)),
	'$trace'(1).

% ----------------
notrace :-
	'$trace'(-1),
	retractall('$trmode'(_)).

% ----------------
debug(Goal) :-
	'$get_global'('$can_debug', true), !,
	'$set_global'('$can_debug', false),
	debug_do(Goal),
	'$set_global'('$can_debug', true).
debug(Goal) :-
	'$pred_info'(Goal, _, access_user), !,
	debug_do(Goal).
debug(Goal) :-
	'$trace'(0),
	Goal.

% ----------------
debug_do(Goal) :-
	new_invocation(Info),
	debug_try(Info, Goal).

% ----------------
new_invocation(InvNum-Level) :-
	'$get_global'('$debug_level', Level),
%	retract('$invocation_num'(InvNum)),
%	InvNum1 is InvNum + 1,
%	asserta('$invocation_num'(InvNum1)).
	'$get_global'('$invocation_num', InvNum0),
	InvNum is InvNum0 + 1,
	'$set_global'('$invocation_num', InvNum).
	
% ----------------
debug_try(Info, Goal) :-
	'execution:catch_some'(debug_enter(Info, Goal), '$retry',
	                       debug_try(Info, Goal), ['$abort']).

debug_enter(Info, Goal) :-
	enter(Info, Goal),
	Info = _-Level,
	Level1 is Level + 1,
	'$set_global'('$debug_level', Level1),
	catch(debug_call(Goal), E, exception(Info, Goal, E)),
	'$set_global'('$debug_level', Level),
	leave(Info, Goal).

% ----------------
enter(Info, Goal) :-
	port(call, Info, Goal).
enter(Info, Goal) :-
	restore_skip(Info),
	port(fail, Info, Goal),
	fail.

% ----------------
leave(Info, Goal) :-	
	restore_skip(Info),
	port(exit, Info, Goal).
leave(Info, Goal) :-
        port(redo, Info, Goal),
	fail.

% ----------------
exception(Info, Goal, E) :-
	restore_skip(Info),
	port(exception, Info, Goal-E),
	throw(E).

% ----------------
debug_call(Goal) :-
	'$pred_info'(Goal, _, Access),
	debug_type(Access, Goal).

% ----------------
debug_type(access_prolog, Goal) :-
	'$trace'(0),
	Goal.
debug_type(access_user, Goal) :-
	'$set_global'('$can_debug', true),
	'$trace'(0),
	Goal,
	'$set_global'('$can_debug', false).

% ----------------
port(Port, Info, Goal) :-
	get_mode(creep), !,
	'io:default_streams'(port_action(Port, Info, Goal)).
port(_, _, _).

% ----------------
port_action(Port, Info, Goal) :-
	write_debug(Port, Info, Goal),
	prompt(X),
	deb_action(X, Port, Info).

% ----------------
deb_action(0'a, _, _) :-
	!, 'execution:abort'.
deb_action(0's, Port, Info) :-
	in_port(Port),
	!,
	skip_this(Info).
deb_action(0'r, _Port, _) :-
	!,
	throw('$retry').
deb_action(0'l, _, _) :-
	!, 
	set_mode(leap).
deb_action(_, _, _).

in_port(call).
in_port(redo).

% ----------------
skip_this(Info) :-
	Info = InvNum-_,
	asserta('$skip_invocation'(InvNum)),
	set_mode(skip).

restore_skip(Info) :-
	Info = InvNum-_,
	retract('$skip_invocation'(InvNum)), !,
	set_mode(creep).
restore_skip(_).

% ----------------
write_debug(Port, Info, Goal) :-
	Info = InvNum-Level,
	write(InvNum), put_code(0'\t),
	write(Level), put_code(0'\t),
	port_name(Port, Name),
	write(Name),  write(': '),
	write_goal(Port, Goal), write(' ? ').

% ----------------
write_goal(exception, Goal-E) :- !,
	write(E), write(' - '), write(Goal).
write_goal(_, Goal) :-
	write(Goal).

% ----------------
port_name(call,      'Call').
port_name(exit,      'Exit').
port_name(fail,      'Fail').
port_name(redo,      'Redo').
port_name(exception, 'Exception').

% ----------------
prompt(X) :-
	'io:read_line'([X|_]).

% ----------------
set_mode(Mode) :-
	retract('$trmode'(_)), !,
	asserta('$trmode'(Mode)).
set_mode(_).

% ----------------
get_mode(Mode) :-
	clause('$trmode'(Mode), _).	

% ----------------
reset :-
%	retractall('$invocation_num'(_)),
%	asserta('$invocation_num'(0)),
	'$set_global'('$invocation_num', 0),
	'$set_global'('$debug_level', 1).


