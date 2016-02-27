:- module(char_io, [get_code/1, get_code/2, 
		    get_char/1, get_char/2, 
		    get_byte/1, get_byte/2,
		    peek_code/1, peek_code/2,
		    peek_char/1, peek_char/2,
		    peek_byte/1, peek_byte/2,
		    put_code/1, put_code/2,
		    put_char/1, put_char/2,
		    put_byte/1, put_byte/2,
		    nl/0, nl/1]).

:- use_module(io, [handle_eof/1, with_input/4, with_output/4, 
		   check_input_text/0, check_input_binary/0,
		   check_output_text/0, check_output_binary/0]).
:- use_module(err, [err_check/2]).

% ----------------

getc(C) :-
	'$get_code'(C), !.
getc(C) :-
	handle_eof(get),
	C = -1.

peekc(C) :-
	'$peek_code'(C), !.
peekc(C) :-
	handle_eof(peek),
	C = -1.

% ----------------
check_in_code(C) :-
	var(C).
check_in_code(C) :-
	integer(C),
	( C = -1 ; C > 0, C < 256 ).

check_in_char(CH) :-
	var(CH).
check_in_char(CH) :-
	atom(CH), 
	( CH = end_of_file ; '$atom_codes'(CH, [_]) ).

check_in_byte(B) :-
	var(B).
check_in_byte(B) :-
	integer(B),
	B >= -1, B < 256.

check_ret_code(0) :- !, fail.
check_ret_code(_).

% ----------------
get_code_char(-1, end_of_file) :- !.
get_code_char(Code, Char) :-
	atom_codes(Char, [Code]).
	

% ----------------
get_code(C) :-
	check_in_code(C), 
	check_input_text,
	getc(C0), 
	check_ret_code(C0), !,
	C = C0.
get_code(C) :-
	err_check(get_code(C), 
	            [integer(C),
		     in_char_code(C),
		     do(current_input(S)),
		     s_text_binary(input, text, S),
		     input_character]).

% ----------------
get_char(CH) :-
	check_in_char(CH),
	check_input_text,
	getc(C0),
	check_ret_code(C0), !,
	get_code_char(C0, CH).
get_char(CH) :-
	err_check(get_char(CH), 
	            [in_char(CH),
		     do(current_input(S)),
		     s_text_binary(input, text, S),
		     input_character]).

% ----------------
get_byte(B) :-
	check_in_byte(B), 
	check_input_binary,
	getc(B0), !,
	B = B0.
get_byte(B) :-
	err_check(get_byte(B), 
	            [in_byte(B),
		     do(current_input(S)),
		     s_text_binary(input, binary, S)]).

% ----------------
peek_code(C) :-
	check_in_code(C),
	check_input_text,
	peekc(C0),
	check_ret_code(C0), !,
	C = C0.
peek_code(C) :-
	err_check(peek_code(C), 
	            [integer(C),
		     in_char_code(C),
		     do(current_input(S)),
		     s_text_binary(input, text, S),
		     input_character]).

% ----------------
peek_char(CH) :-
	check_in_char(CH),
	check_input_text,
	peekc(C0),
	check_ret_code(C0), !,
	get_code_char(C0, CH).
peek_char(CH) :-
	err_check(peek_char(CH), 
	            [in_char(CH),
		     do(current_input(S)),
		     s_text_binary(input, text, S),
		     input_character]).

% ----------------
peek_byte(B) :-
	check_in_byte(B), 
	check_input_binary,
	peekc(B0), !,
	B = B0.
peek_byte(B) :-
	err_check(peek_byte(B), 
	            [in_byte(B),
		     do(current_input(S)),
		     s_text_binary(input, binary, S)]).

% ----------------
get_code(S, C) :-
	check_in_code(C), 
	with_input(S, text, getc(C0), get_code(S, C)), 
	check_ret_code(C0), !,
	C = C0.
get_code(S, C) :-
	err_check(get_code(S, C), 
	            [integer(C),
		     in_char_code(C),
		     input_character]).

% ----------------
get_char(S, CH) :-
	check_in_char(CH),
	with_input(S, text, getc(C0), get_char(S, CH)),
	check_ret_code(C0), !,
	get_code_char(C0, CH).
get_char(S, CH) :-
	err_check(get_char(S, CH), 
	            [in_char(CH),
		     input_character]).

% ----------------
get_byte(S, B) :-
	check_in_byte(B),
	with_input(S, binary, getc(B0), get_byte(S, B)), !,
	B = B0.
get_byte(S, B) :-
	err_check(get_byte(S, B), 
	            [in_byte(B)]).

% ----------------
peek_code(S, C) :-
	check_in_code(C), 
	with_input(S, text, peekc(C0), peek_code(S, C)), 
	check_ret_code(C0), !,
	C = C0.
peek_code(S, C) :-
	err_check(peek_code(S, C), 
	            [integer(C),
		     in_char_code(C),
		     input_character]).

% ----------------
peek_char(S, CH) :-
	check_in_char(CH),
	with_input(S, text, peekc(C0), peek_char(S, CH)),
	check_ret_code(C0), !,
	get_code_char(C0, CH).
peek_char(S, CH) :-
	err_check(peek_char(S, CH), 
	            [in_char(CH),
		     input_character]).

% ----------------
peek_byte(S, B) :-
	check_in_byte(B),
	with_input(S, binary, peekc(B0), peek_byte(S, B)), !,
	B = B0.
peek_byte(S, B) :-
	err_check(peek_byte(S, B), 
	            [in_byte(B)]).

% ----------------
put_code(C) :-
	integer(C),
	C > 0, C < 256,
	check_output_text, !,
	'$put_code'(C).
put_code(C) :-
	err_check(put_code(C),
	          [inst(C),
		   integer(C),
		   char_code(C),
		   do(current_output(S)),
		   s_text_binary(output, text, S)]).

% ----------------
put_char(CH) :-
	'$atom_codes'(CH, [C]),
	check_output_text, !,
	'$put_code'(C).
put_char(CH) :-
	err_check(put_char(CH),
	          [inst(CH),
		   character(CH),
		   do(current_output(S)),
		   s_text_binary(output, text, S)]).

% ----------------
put_byte(B) :-
	integer(B), 
	B >= 0, B < 256,
	check_output_binary, !,
	'$put_code'(B).
put_byte(B) :-
	err_check(put_byte(B),
	          [inst(B),
		   byte(B),
		   do(current_output(S)),
		   s_text_binary(output, binary, S)]).

% ----------------
put_code(S, C) :-
	integer(C),
	C > 0, C < 256, !,
	with_output(S, text, '$put_code'(C), put_code(S, C)).
put_code(_S, C) :-
	err_check(put_code(C),
	          [inst(C),
		   integer(C),
		   char_code(C)]).

% ----------------
put_char(S, CH) :-
	'$atom_codes'(CH, [C]), !,
	with_output(S, text, '$put_code'(C), put_char(S, CH)).
put_char(S, CH) :-
	err_check(put_char(S, CH), 
	          [inst(CH),
		   character(CH)]).

% ----------------
put_byte(S, B) :-
	integer(B), 
	B >= 0, B < 256, !, 
	with_output(S, binary, '$put_code'(B), put_byte(S, B)).
put_byte(S, B) :-
	err_check(put_byte(S, B), 
	          [inst(B),
		   byte(B)]).

% ----------------
nl :-
	check_output_text, !,
	'$put_code'(0'\n).
nl :-
	err_check(nl, 
	          [do(current_output(S)),
		   s_text_binary(output, text, S)]).

% ----------------
nl(S) :-
	with_output(S, text, '$put_code'(0'\n), nl(S)).

