:- module(toplevel, []).

put_result(Vars):-
	write_vars(Vars, N),
	choose_continue(N).

write_result(AssignList, NumVars0, NumVars1) :-
	name_vars(AssignList), %%%% pts %%%%
	write_result_low(AssignList, NumVars0, NumVars1).

%%%% pts %%%%
name_vars([]).
name_vars([Name=Var|VN]) :-
	( var(Var) -> Var='$VAR'(Name)
	; true
	),
	name_vars(VN).

write_result_low([], N, N).
write_result_low([Name=Var|VN], N, NR):-
	Var \== '$VAR'(Name), !, %%%% pts %%%%
	% !! ^^^ write variables if they are equal to others
	nl, write(Name), write(' = '),
	( 'write:write_style'(Var, style(true,false,atom)) -> true % Dat: write '$VAR'('VarName') as VarName
        ; write('!writeq_failed!'), write(Var) %%%% pts %%%%
        ),
	N1 is N + 1,
	write_result_low(VN, N1, NR).
write_result_low([_|VN], N, NR):-
	write_result_low(VN, N, NR).

finished(0):-!.
finished(_):-
	'io:default_streams'((write(' ? '), 'io:read_line'([X|_]))),
	X \= 59.

result(VN):-
	'io:default_streams'((write_result(VN, 0, N))),
	finished(N).


% ----------------
interrupt_actions(0'a):-
	'execution:abort'.
interrupt_actions(_).

interrupt_ask(X) :-
	nl, write('Prolog interruption. ? '),
	'io:read_line'([X|_]).

interrupt :-
	'io:default_streams'(interrupt_ask(X)),
	interrupt_actions(X).

	
% ----------------
try(X, _) :-
	X == end_of_file, !,
	'io:default_streams'(nl),
	%%%% pts %%%% vvv 'toplevel:exitcode', so exit code is nonzero if the last normal call didn't succeed
	( '$get_bb'('toplevel:exitcode', Code) -> true
	; Code=0
	),
	halt(Code).
try(X, VN) :-
	% write(tc(X)), nl,
	'$set_bb'('toplevel:exitcode', 0), %%%% pts %%%%
	'execution:toplevel_call'(X),
	result(VN), !,
	'io:default_streams'((nl, write(yes))).
try(_, _) :-
	'$set_bb'('toplevel:exitcode', 2), %%%% pts %%%%
	'io:default_streams'((nl, write(no))).

% ----------------
toplevel_read(Term, VN) :-
	(    clause('$trmode'(_), _) -> nl, write('{trace}')
        ;    true 
        ),
	nl, write('?- '),
	read_term(Term, [variable_names(VN)]),
	( Term \== end_of_file -> get_code(_) ; true).   % read final new-line

top_level :-
	'io:default_streams'(toplevel_read(Term, VN)),
	try(Term, VN).

% ----------------
handle_exception(Stream,E) :-
	'$set_bb'('toplevel:exitcode', 3), %%%% pts %%%%
	nl(Stream),
	'execution:write_exception'(Stream, E),
	nl(Stream). %%%% pts %%%%

% ----------------
main :-
	'execution:init_builtins',
	write(user_error, 'Welcome to the toplevel of '),
	( current_prolog_flag(version, Version) -> true % Version='aprolog X.YY'
	; Version='this prolog'
	),
	write(user_error, Version), nl(user_error),
	write(user_error, 'The license is GNU GPL >=2.0. It comes without warranty. USE AT YOUR OWN RISK!'), nl(user_error),
	% write(user_error, 'This program is copyright (C) Miklos Szeredi'), nl(user_error), % ... and many other people -- see the README.txt for the list
	repeat,
	%%%% pts %%%% vvv Dat: both SICStus and SWI-Prolog write exceptions to user_error -- so do we.
        'execution:catch_some'(top_level, E, handle_exception(user_error, E), []),
	fail.
