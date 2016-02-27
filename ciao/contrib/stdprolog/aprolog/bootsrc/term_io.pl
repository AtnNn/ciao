:- module(term_io, [read/1, read/2, read_term/2, read_term/3,
	            write/1, write/2, 
		    writeq/1, writeq/2,
		    write_canonical/1, write_canonical/2,
		    write_term/2, write_term/3,
		    op/3, current_op/3,
		    char_conversion/2, current_char_conversion/2]).

:- use_module(io, [handle_eof/1, with_input/4, with_output/4, 
	           check_input_text/0, check_output_text/0]).
:- use_module(err, [err_check/2]).

% ----------------
read(T) :-
	check_input_text, !,
	'read:read_with_vars'(T,_).
read(T) :-
	err_check(read(T),
	          [do(current_input(S)),
		   s_text_binary(input, text, S)]).

% ----------------
read(S, T) :-
	with_input(S, text, 'read:read_with_vars'(T,_), read(S, T)).

% ----------------
read_term(T, O) :-
	check_input_text, !,
	read_term_i(T, O).
read_term(T, O) :-
	err_check(read_term(T, O),
	          [do(current_input(S)),
		   s_text_binary(input, text, S)]).

% ----------------
read_term(S, T, O) :-
	with_input(S, text, read_term_i(T, O), read_term(S, T, O)).


% ----------------
read_term_i(T, O) :-
	nonvar(O),
	check_read_opts(O), !,
	'read:read_with_vars'(T, Vars),
	read_term_opts(O, Vars).
read_term_i(T, O) :-
	err_check(read_term(T, O),
	          [inst(O),
		   list(O),
		   instlist(O),
		   instlistelements(O)]).

check_read_opts([]).
check_read_opts([Opt|Opts]):-
	nonvar(Opt),
	nonvar(Opts),
	check_read_opt(Opt),
	check_read_opts(Opts).
	
check_read_opt(variable_names(_)) :- !.
check_read_opt(variables(_)) :- !.
check_read_opt(singletons(_)) :- !.
check_read_opt(Opt) :-
	throw(error(domain_error(read_option, Opt), read_term)).

read_term_opts([], _).
read_term_opts([Opt|Opts], Vars) :-
	read_term_opt(Opt, Vars),
	read_term_opts(Opts, Vars).

read_term_opt(variable_names(VN), Vars) :- 
	get_var_names(Vars, [], VN).
read_term_opt(variables(VL), Vars) :-
	get_var_list(Vars, [], VL).
read_term_opt(singletons(VS), Vars) :-
	get_var_singeltons(Vars, [], VS).

get_var_names([], VN, VN).
get_var_names([NameVar-_|Vars], VN0, VN) :-
	(   NameVar = ('_'=_) -> VN1 = VN0
        ;   VN1 = [NameVar|VN0]
        ),
	get_var_names(Vars, VN1, VN).

get_var_list([], VL, VL).
get_var_list([(_=V)-_|Vars], VL0, VL) :-
	get_var_list(Vars, [V|VL0], VL).

get_var_singeltons([], VS, VS).
get_var_singeltons([NameVar-Singleton|Vars], VS0, VS) :-
	(   NameVar = ('_'=_) -> VS1 = VS0
        ;   nonvar(Singleton) -> VS1 = VS0
        ;   VS1 = [NameVar|VS0]
        ),
	get_var_singeltons(Vars, VS1, VS).

% ----------------

write_i(T) :-
	'write:write_style'(T, style(false,false,true)).

write(T) :-
	check_output_text, !,
	write_i(T).
write(T) :-
	err_check(write(T),
	          [do(current_output(S)),
		   s_text_binary(output, text, S)]).

write(S, T) :-
	with_output(S, text, write_i(T), write(S, T)).

% ----------------
writeq_i(T) :-
	'write:write_style'(T, style(true,false,true)).

writeq(T) :-
	check_output_text, !,
	writeq_i(T).
writeq(T) :-
	err_check(writeq(T),
	          [do(current_output(S)),
		   s_text_binary(output, text, S)]).

writeq(S, T) :-
	with_output(S, text, writeq_i(T), writeq(S, T)).

% ----------------
write_canonical_i(T) :-
	'write:write_style'(T, style(true,true,false)).

write_canonical(T) :-
	check_output_text, !,
	write_canonical_i(T).
write_canonical(T) :-
	err_check(write_canonical(T),
	          [do(current_output(S)),
		   s_text_binary(output, text, S)]).

write_canonical(S, T) :-
	with_output(S, text, write_canonical_i(T), write_canonical(S, T)).

% ----------------
write_term(T, Opts) :-
	check_output_text, !,
	write_term_i(T, Opts).
write_term(T, Opts) :-
	err_check(write_term(T, Opts),
	          [do(current_output(S)),
		   s_text_binary(output, text, S)]).

write_term(S, T, Opts) :-
	with_output(S, text, write_term_i(T, Opts), write_term(S, T, Opts)).

% ----------------
write_term_i(T, O) :-
	nonvar(O),
	write_opts(O, style(false, false, false), Style), !,
	'write:write_style'(T, Style).
write_term_i(T, O) :-
	err_check(write_term(T, O),
	          [inst(O),
		   list(O),
		   instlist(O),
		   instlistelements(O)]).

write_opts([], Style, Style).
write_opts([Opt|Opts], Style0, Style):-
	nonvar(Opt),
	nonvar(Opts),
	write_opt(Opt, Style0, Style1),
	write_opts(Opts, Style1, Style).

write_opt(quoted(Val),     style(_, SC, SN), style(Val, SC, SN)) :-
	check_bool(Val), !.
write_opt(ignore_ops(Val), style(SQ, _, SN), style(SQ, Val, SN)) :-
	check_bool(Val), !.
write_opt(numbervars(Val), style(SQ, SC, _), style(SQ, SC, Val)) :-
	check_bool(Val), !.	
write_opt(Opt, _, _) :-
	throw(error(domain_error(write_option, Opt), write_term)).

check_bool(X) :-
	nonvar(X),
	( X = true ; X = false ).

% ----------------
current_op(Prec, Type, Op) :-
	( var(Prec) ; integer(Prec), Prec >= 0, Prec =< 1200 ),
	( var(Type) ; op_class(Type, _) ),
	( var(Op)   ; atom(Op) ), !,
	clause('$operator'(Op, Type, Prec), _).
current_op(Prec, Type, Op) :-
	err_check(current_op(Prec, Type, Op), %%%% pts %%%% BUGFIX: changed Ops to Op
	            [integer(Prec),
		     op_prec(Prec),
		     atom(Type),
		     op_type(Type),
		     atom(Op)
		    ]).

% ----------------
op(Prec, Type, Ops) :-
	integer(Prec),
	Prec >= 0, Prec =< 1200,
	nonvar(Type),
	op_class(Type, _),
	nonvar(Ops),
	check_ops(Ops, Type, OpList), !,
	modify_oplist(OpList, Type, Prec).
op(Prec, Type, Ops) :-
	err_check(op(Prec, Type, Ops),
	            [inst(Prec),
		     inst(Type),
		     inst(Ops),
		     integer(Prec),
		     atom(Type),
		     op_prec(Prec),
		     op_type(Type),
		     list(Ops),
		     instlist(Ops)
		    ]).

op_i(Prec, Type, Ops) :-
	modify_oplist(Ops, Type, Prec).

% ----------------
modify_oplist([], _, _).
modify_oplist([Op|Ops], Type, Prec) :-
	modify_op(Op, Type, Prec),
	modify_oplist(Ops, Type, Prec).

modify_op(Op, Type, Prec) :-
	remove_op(Op, Type),
	add_op(Prec, Type, Op).

add_op(0, _, _) :- !.
add_op(Prec, Type, Op) :-
        assertz('$operator'(Op, Type, Prec)).

remove_op(Op, Type) :-
	op_class(Type, Class),
	op_class(TypeSim, Class),
	retract('$operator'(Op, TypeSim, _)), !.
remove_op(_, _).

% ----------------

check_ops([], _, []) :- !.
check_ops([Op|S], Type, [Op|S]) :- !,
	check_oplist([Op|S], Type).
check_ops(Op, Type, [Op]) :-
	atom(Op),
	check_op(Op, Type).

check_oplist([], _).
check_oplist([Op|Ops], Type) :-
	nonvar(Ops),
	check_op(Op, Type),
	check_oplist(Ops, Type).

check_op(Op, Type) :-
	atom(Op),
	Op \= ',',
	\+ op_conflict(Op, Type), !.
check_op(Op, Type) :-
	err_check(op(_, Type, Op),
	            [inst(Op),
		     atom(Op),
		     op_modify(Op),
		     op_create(Op, Type)]).
op_conflict(Op, Type) :-
	op_class(Type, Class),
	clause('$operator'(Op, OldType, _), _),
	op_class(OldType, OldClass),
	class_conflict(Class, OldClass), !.

class_conflict(infix, postfix).
class_conflict(postfix, infix).

% ----------------
op_class(fx,  prefix).
op_class(fy,  prefix).
op_class(xf,  postfix).
op_class(yf,  postfix).
op_class(xfx, infix).
op_class(yfx, infix).
op_class(xfy, infix).

% ----------------

init_ops :-
	op_i( 1200, xfx, [ :-, --> ]),
	op_i( 1200,  fx, [ :-, ?- ]),
	op_i( 1100, xfy, [ ; ]),
	op_i( 1050, xfy, [ -> ]),
	op_i( 1000, xfy, [ ',' ]),
	op_i(  900,  fy, [ \+ ]),
	op_i(  700, xfx, [ =, \=, is, =.., ==, \==, @<, @>, @=<, @>=,
			 =:=, =\=, <, >, =<, >= ]),
	op_i(  500, yfx, [ +, -, /\, \/ ]),
	op_i(  400, yfx, [ *, /, //, mod, rem, <<, >> ]),
	op_i(  200, xfx, [ ** ]),
	op_i(  200, xfy, [ ^ ]),
	op_i(  200,  fy, [ -, \ ]).


% ----------------
char_conversion(IC, OC) :-
	is_char(IC),
	is_char(OC), !,
	char_code(IC, ICC),
	char_code(OC, OCC),
	remove_char_conv(ICC),
	add_char_conv(ICC, OCC),
	check_char_conv_state.
char_conversion(IC, OC) :-
	err_check(char_conversion(IC, OC),
	          [inst(IC),
		   character(IC),
		   inst(OC),
		   character(OC)]).

remove_char_conv(IC) :-
	retract('$char_conv'(IC, _)), !.
remove_char_conv(_).
	
add_char_conv(IC, IC) :- !.
add_char_conv(IC, OC) :-
	asserta('$char_conv'(IC, OC)).
	
% ----------------
current_char_conversion(IC, OC) :-
	( var(IC) ; is_char(IC) ),
	( var(OC) ; is_char(OC) ), !,
	'$char_conv'(ICC, OCC),
	char_code(IC, ICC),
	char_code(OC, OCC).
current_char_conversion(IC, OC) :-
	err_check(current_char_conversion(IC, OC),
	          [character(IC),
		   character(OC)]).

% ----------------
check_char_conv_state :-
	current_prolog_flag(char_conversion, on),
	'$char_conv'(_,_), !,
	'$abolish'('$conv_char'(_,_)),
	'$asserts'('$conv_char'(A,B), [do_conv_char(A,B)], _).
check_char_conv_state :-
	'$abolish'('$conv_char'(_,_)),
	'$asserts'('$conv_char'(A,A), [], _).
	
do_conv_char(IC, OC) :-
	'$char_conv'(IC, OC), !.
do_conv_char(IC, IC).

% ----------------
init_charconv :-
	asserta('$char_conv'(_,_)),
	retract('$char_conv'(_,_)),
	check_char_conv_state.

% ----------------
is_char(C) :-
	atom(C),
	atom_codes(C, [_]).

	
