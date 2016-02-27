:- module(err, []).

:- use_module(lists, [list_or_partial_list/1]).

err_check(Call, []) :-
	throw(error(unknown_error, Call)).
err_check(Call, [Check|Checks]) :-
	check_one(Check, Call), !,
	err_check(Call, Checks).

check_one(do(Something), _) :-
	( call(Something) ; true ).
check_one(if(Cond, Check), Call) :-
	call(Cond), !, 
	check_one(Check, Call).
check_one(inst(X), Call) :-
	var(X),
	throwerror(instantiation_error, Call).
check_one(var(X), Call) :-
	nonvar(X),
	throwerror(type_error(variable,X), Call).
check_one(integer(X), Call) :-
	nonvar(X),
	\+ integer(X),
	throwerror(type_error(integer,X), Call).
check_one(number(X), Call) :-
	nonvar(X),
	\+ number(X),
	throwerror(type_error(number,X), Call).
check_one(atom(X), Call) :-
	nonvar(X),
	\+ atom(X),
	throwerror(type_error(atom,X), Call).
check_one(atomic(X), Call) :-
	nonvar(X),
	\+ atomic(X),
	throwerror(type_error(atomic,X), Call).
check_one(compound(X), Call) :-
	nonvar(X),
	\+ compound(X),
	throwerror(type_error(compound,X), Call).
check_one(callable(X), Call) :-
	nonvar(X),
	\+ callable(X),
	throwerror(type_error(callable,X), Call).
check_one(conv_to_goal(X), Call) :-
	nonvar(X),
	\+ 'execution:convert_to_goal'(X, _),
	throwerror(type_error(callable,X), Call).
check_one(character(X), Call) :-
	nonvar(X),
	\+ is_char(X),
        throwerror(type_error(character, X), Call).
check_one(byte(X), Call) :-
	nonvar(X),
	\+ ( integer(X), X >= 0, X < 256 ),
        throwerror(type_error(byte, X), Call).
check_one(list(X), Call) :-
	\+ list_or_partial_list(X),
	throwerror(type_error(list,X), Call).
check_one(instlist(X), Call) :-
	\+ is_list(X),
	throwerror(instantiation_error, Call).
check_one(instlistelements(X), Call) :-
	\+ inst_list_elements(X),
	throwerror(instantiation_error, Call).
check_one(nonemptylist(X), Call) :-
	nonvar(X),
	X = [],
	throwerror(domain_error(non_empty_list, X), Call).
check_one(pred_ind(X), Call) :-
	nonvar(X),
	X \= _/_,
	throwerror(type_error(predicate_indicator, X), Call).
check_one(pred_ind_strict(X), Call) :-
	\+ 'database:check_pred_ind'(X),
	throwerror(type_error(predicate_indicator, X), Call).
check_one(nonneg(X), Call) :-
	integer(X),
	X < 0,
	throwerror(domain_error(not_less_than_zero,X), Call).
check_one(max_arity(X), Call) :-
	integer(X),
	current_prolog_flag(max_arity, MaxArity),
	X > MaxArity,
	throwerror(representation_error(max_arity), Call).
check_one(op_prec(X), Call) :-
	integer(X),
	( X < 0 ; X > 1200 ),
	throwerror(domain_error(operator_priority, X), Call).
check_one(op_type(X), Call) :-
	\+ 'term_io:op_class'(X, _),
	throwerror(domain_error(operator_specifier, X), Call).	
check_one(op_modify(Op), Call) :-
	nonvar(Op),
	Op = ',',
	throwerror(permission_error(modify, operator, Op), Call).
check_one(op_create(Op, Type), Call) :-
	'term_io:op_conflict'(Op, Type),
	throwerror(permission_error(create, operator, Op), Call).
check_one(source_sink(X), Call) :-
	nonvar(X),
	\+ atom(X),
	throwerror(domain_error(source_sink, X), Call).
check_one(stream(X), Call) :-
	\+ 'io:check_stream'(X),
	throwerror(domain_error(stream, X), Call).
check_one(s_or_a(X), Call) :-
	\+ 'io:stream_to_num'(X, _SN),
	throwerror(domain_error(stream_or_alias, X), Call).
check_one(s_exist(X), Call) :-
	'io:stream_to_num'(X, SN),
	\+ 'io:stream_type'(SN, _, _),
	throwerror(existence_error(stream, X), Call).
check_one(s_in_out(IO, X), Call) :-
	'io:stream_to_num'(X, SN),
	'io:stream_type'(SN, IOR, _),
	IOR \= IO,
	throwerror(permission_error(IO, stream, X), Call).
check_one(s_text_binary(IO, TB, X), Call) :-
	'io:stream_to_num'(X, SN),
	'io:stream_type'(SN, _, TBR),
	TB \= TBR,
	(   TB = text -> TBS = binary_stream
	;   TBS = text_stream
	),
	throwerror(permission_error(IO, TBS, X), Call).
check_one(stream_property(X), Call) :-
	\+ 'io:check_property'(X),
	throwerror(domain_error(stream_property, X), Call).
check_one(s_reposition(S), Call) :-
	\+ stream_property(S, reposition(true)),
	throwerror(permission_error(reposition, stream, S), Call).
check_one(position(Pos), Call) :-
	\+ 'io:check_stream_position'(Pos, _),
	throwerror(domain_error(stream_position, Pos), Call).
check_one(in_char_code(C), Call) :-
	\+ 'char_io:check_in_code'(C), 
	throwerror(representation_error(in_character_code), Call).
check_one(char_code(C), Call) :-
	\+ is_char_code(C), 
	throwerror(representation_error(character_code), Call).
check_one(char_list(C), Call) :-
	nonvar(C),
	\+ check_char_list(C, Call).
check_one(char_code_list(C), Call) :-
	nonvar(C),
	\+ is_char_code_list(C),
	throwerror(representation_error(character_code), Call).
check_one(in_char(C), Call) :-
	\+ 'char_io:check_in_char'(C),
	throwerror(type_error(in_character, C), Call).
check_one(in_byte(C), Call) :-
	\+ 'char_io:check_in_byte'(C),
	throwerror(type_error(in_byte, C), Call).
check_one(input_character, Call) :-
	/* Cannot check */
        throwerror(representation_error(character), Call).
check_one(private_procedure(X), Call) :-
	\+ '$pred_info'(X, pred_dynamic, _),
	functor(X, N, A),
	throwerror(permission_error(access,private_procedure,N/A), Call).
check_one(static_procedure(X), Call) :-
	\+ '$pred_info'(X, pred_dynamic, _),
	functor(X, N, A),
	throwerror(permission_error(modify,static_procedure,N/A), Call).
check_one(prolog_flag(X), Call) :-
	\+ 'execution:flag_ok'(X),
	throwerror(domain_error(prolog_flag, X), Call).
check_one(prolog_flag_modify(X), Call) :-
	\+ 'execution:flag_modifiable'(X),
	throwerror(permission_error(modify, flag, X), Call).
check_one(prolog_flag_value(X, Y), Call) :-
	\+ 'execution:flag_value_ok'(X, Y),
	throwerror(domain_error(flag_value, X+Y), Call).
check_one(_, _).


throwerror(ISOErr, Call) :-
	throw(error(ISOErr, Call)).

is_list([]).
is_list([_|L]) :-
	nonvar(L),
	is_list(L).

inst_list_elements([]).
inst_list_elements([X|L]) :-
	nonvar(X),
	inst_list_elements(L).

is_char_code(C) :-
	integer(C), C > 0, C < 256.

is_char(C) :-
	atom(C),
	atom_codes(C, [_]).

is_char_code_list([]).
is_char_code_list([C|L]) :-
	nonvar(L),
	is_char_code(C),
	is_char_code_list(L).

check_char_list([], _).
check_char_list([CH|L], Call) :-
	nonvar(L),
	check_char(CH, Call),
	check_char_list(L, Call).

check_char(CH, _) :-
	is_char(CH), !.
check_char(CH, Call) :-
	throwerror(type_error(character, CH), Call).
