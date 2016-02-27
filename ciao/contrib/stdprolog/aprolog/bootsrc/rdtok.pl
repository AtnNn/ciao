/* Simple lexical analyser */

:- module(rdtok, []).
:- use_module(io, [handle_eof/1]).
:- use_module(lists, [member/2]).

/* Private get_code/1, peek_code/1 without stream type checking */
get_code(C) :-
        '$get_code'(C), !.
get_code(-1).

peek_code(C) :-
	'$peek_code'(C), !.
peek_code(-1).

conv_char(UC, C) :-
	'$conv_char'(UC, C).

get_code_conv(C) :-
	get_code(UC),
	conv_char(UC, C).

peek_code_conv(C) :-
	peek_code(UC),
	conv_char(UC, C).

number_codes(N, L) :-
	'$number_codes'(N, L).
	
/* read_tokens(L): L is the list of tokens on the input stream terminated */
/* by end_of_file, or a '.' followed by layout.*/

read_tokens(L, VL):-
	get_code(C),
	read_token(C, T, [], VL0, C1),
	(   T = end_of_file ->
	    handle_eof(get),
	    L = [atom(end_of_file)], VL = []
	;   read_tokens(T, C1, VL0, VL, L)
	).

/* read_tokens(C, T, L): L is the list of tokens on the input stream, */
/* where C is the first character of the input stream and T is the last */
/* token read */

read_tokens(end_of_file, _, _, _, _):-
	throw(error(syntax_error('End of file in expression'), read/1)).
read_tokens(end_of_term, _C, VL0, VL, L):-
	!, L = [], VL = VL0.
read_tokens(T, C, VL0, VL, [T|L]) :-
	read_token(C, T1, VL0, VL1, C1), read_tokens(T1, C1, VL1, VL, L).


/* read_token(C, T, C1): T is the next tokens on the input stream, */
/* where C is the first character of the input stream and C1 is the first  */
/* character following the token T. */

read_token(C0, T, VL0, VL, C):-
	read_token(C0, CL, Type, C),
	(   is_var_token(Type, CL) ->
	    build_var_token(CL, VL0, VL, T)
	;   build_token(Type, CL, C0, T),
	    VL = VL0
	).

is_var_token(Type, CL) :-
	Type = letter, CL =[C1|_], ( 0'A =< C1, C1 =< 0'Z -> true; C1 = 0'_).

build_var_token(CL, VL0, VL, var(Var,VarName)) :-
	atom_codes(VarName, CL),
	add_var(VarName, Var, VL0, VL).

add_var(Name, Var, VL0, VL) :-
	Name \= '_', member((Name=Var)-notsingleton, VL0), !, VL = VL0.
add_var(Name, Var, VL0, [(Name=Var)-_Singleton|VL0]).

build_token(quote(0'") /*"*/, CL, _, T) :- !, T = string(CL).
build_token(digit, CL, _, T) :- !, number_codes(N, CL), T = number(N).
build_token(number, N, _, T) :- !, T = number(N).
build_token(verb(T0), _, _, T) :- !, T = T0.
build_token(punct, [0'(], C0, T) :-
	char_type(C0, layout), !, T = ' ('.
build_token(punct, CL, _, T) :-
	!, atom_codes(T, CL).
build_token(error(Err), CL, _, error(Err, Atom)) :- !,
	atom_codes(Atom, CL).
build_token(_, CL, _, atom(Atom)) :-
	atom_codes(Atom, CL).

/* read_token(C0, CL, Type, C1): CL is the char list of the next token, */
/* where C is the first character of the input stream and C1 is the first  */
/* character following the token. */

read_token(C0, CL, Type, C):-
	conv_char(C0, CC0),
	char_type(CC0, Type0), 
	(   Type0 = eof -> Type = verb(end_of_file), CL = [], C = -1
	;   CC0 = 0'., peek_code_conv(C1), char_type(C1, Type1),
	    eot_succ_type(Type1) ->
	    Type = verb(end_of_term), CL = [], C = -1
	;   get_code(C1),
	    read_token(Type0, C0, C1, CL, Type, C)
	).

read_token(quote, C0, C1, CL, Type, C) :- !,
	read_quoted(C1, C0, CL, C, Err),
	(   Err = '' -> Type = quote(C0)
	;   Type = error(Err)
	).
read_token(layout, _C0, C1, CL, Type, C) :- !, 
	read_token(C1, CL, Type, C).
read_token(digit, C0, C1, N, Type, C) :-
	conv_char(C0, 0'0),
	conv_char(C1, CC1),
	read_number_tail(CC1, C1, N, C), !,
	Type = number.
read_token(_Type0, C0, C1, CL, Type, C) :-
	conv_char(C0, CC0),
	conv_char(C1, CC1),
	read_comment(CC0, CC1), !, get_code(C2), 
	read_token(C2, CL, Type, C).
read_token(Type, C0, C1, [CC0|CL], Type, C) :-
	conv_char(C0, CC0),
	read_token_tail(Type, C1, CL, C).

read_number_tail(39, C0, N, C) :- !, %%%% pts %%%% 0'\' for SWI-Prolog
	get_code(C1),
	read_quoted_char(C1, C0, N, C).
read_number_tail(0'b, _, N, C) :- !,
	read_based_number(2, N, C).
read_number_tail(0'o, _, N, C) :- !,
	read_based_number(8, N, C).
read_number_tail(0'x, _, N, C) :- !,
	read_based_number(16, N, C).

read_based_number(B, N, C) :-
	peek_code_conv(C0),
	digit_based(B, C0, V0),
	get_code(_),
	read_based(B, V0, N, C).


read_comment(0'%, C1) :-
        skip_line(C1).
read_comment(0'/, 0'*) :-
	get_code_conv(C1), get_code_conv(C2),
	skip_star_comment(C1, C2).

skip_line(0'\n) :- !.
skip_line(-1) :- !.
skip_line(_):-
	get_code_conv(C),
	skip_line(C).

skip_star_comment(0'*, 0'/) :- !.
skip_star_comment(_, -1) :- !.
skip_star_comment(_, C1) :-
	get_code_conv(C2), skip_star_comment(C1, C2).


/* read_token_tail(Type, C, CL, C1): Given that Type is the char_type of */
/* the first char of the token, C is the next char, CL is the list of */
/* remaining character codes of the next token and C2 is the first char */
/* following the token. */

read_token_tail(Type, C, [CC|CL], C2):-
	conv_char(C, CC),
	char_type(CC, Type1), continued(Type, Type1), !, get_code(C1), 
	read_token_tail(Type, C1, CL, C2).
read_token_tail(digit, C0, [0'.|CL], C2) :-
	conv_char(C0, 0'.),
	peek_code_conv(C1), char_type(C1, digit), !, 
	read_float_tail(CL, C2).
read_token_tail(_Type, C, [], C).



read_quoted(C0, QC, CL, C, Err) :-
	read_quoted_char(C0, QC, N, C1),
	(   N = -1 -> CL = [], C = C1, Err = ''
	;   atom(N) -> Err = N,
	    (   Err = 'newline in quoted' -> CL = [], C = C1
	    ;   read_quoted(C1, QC, CL, C, _)
	    )
	;   CL = [N|CL1],
	    read_quoted(C1, QC, CL1, C, Err)
	).

read_quoted_char(QC, QC, N, C) :- !, 
	get_code(C0),
	(   C0 = QC -> N = QC, get_code(C)
	;   C = C0, N = -1
	).
read_quoted_char(0'\n, _, N, C) :- !,
	get_code(C),
	N = 'newline in quoted'.
read_quoted_char(92, QC, N, C) :- !, %%%% pts %%%% 0'\\ for SWI-Prolog
	get_code(C1),
	(   C1 = 0'\n -> get_code(C2), read_quoted_char(C2, QC, N, C)
	;   read_escape(C1, N, C)
	).
read_quoted_char(C0, _, C0, C) :-
	get_code(C).

read_escape(C0, N, C) :-
	digit_based(8, C0, V0), !,
	read_based(8, V0, N0, C1),
	complete_escape(C1, N0, N, C).
read_escape(0'x, N, C) :-
	get_code(C1),
	digit_based(16, C1, V1), !, 
	read_based(16, V1, N0, C2),
	complete_escape(C2, N0, N, C).
read_escape(C0, N, C) :-
	escape_equiv(C0, N), !,
	get_code(C).
read_escape(C, 'unknown escape', C).

complete_escape(92, N0, N, C) :- %%%% pts %%%% 0'\\ for SWI-Prolog
	N0 < 256, N0 >= 0, !,
	N = N0, 
	get_code(C).
complete_escape(92, _, N, C) :- !, %%%% pts %%%% 0'\\ for SWI-Prolog
	N = 'code too big',
	get_code(C).
complete_escape(C, _, 'missing backslash', C).

read_based(B, V0, V, C) :-
	get_code(C0),
	(   digit_based(B, C0, V1) -> 
	    catch(V2 is V0 * B + V1, error(evaluation_error(int_overflow), _), true),
	    (   nonvar(V2) -> read_based(B, V2, V, C)
	    ;   V = -1, read_based(B, 0, _, C)
	    )
	;   V = V0, C = C0
	).

digit_based(B, C, V) :-
	C >= 0'0, C < 0'0+B, !,
	V is C - 0'0.
digit_based(16, C, V) :-
	C >= 0'a, C =< 0'f, !,
	V is C - 0'a + 10.
digit_based(16, C, V) :-
	C >= 0'A, C =< 0'F, !,
	V is C - 0'A + 10.


read_float_tail(CL1, C) :-
	get_code(C0),
	conv_char(C0, CC0), 
	(   char_type(CC0, digit) -> 
	    CL1 = [CC0|CL], read_float_tail(CL, C)
        ;   (CC0 = 0'e;CC0 =0'E), peek_code_conv(CC1),
	    exponent_head(CC1) -> 
	    CL1 = [CC0,CC1|CL], get_code(_), 
	    read_exponent(CL, C)
	;   CL1 = [], C = C0
        ).

exponent_head(0'+).
exponent_head(0'-).
exponent_head(C) :- char_type(C,digit).

read_exponent(CL, C) :-
	get_code(C0),
	conv_char(C0, CC0),
	(   char_type(CC0, digit) ->
	    CL = [CC0|CL1], read_exponent(CL1, C)
	;   CL = [], C = C0
	).

escape_equiv(0'a, 0'\a).
escape_equiv(0'b, 0'\b).
escape_equiv(0'r, 0'\r).
escape_equiv(0'f, 0'\f).
escape_equiv(0't, 0'\t).
escape_equiv(0'n, 0'\n).
escape_equiv(0'v, 0'\v).
escape_equiv(92, 92). %%%% pts %%%% 0'\\ for SWI-Prolog
escape_equiv(39, 39). %%%% pts %%%% 0'\' for SWI-Prolog
escape_equiv(0'", 0'").
escape_equiv(0'`, 0'`).

/* char_type(C, Type): char code C is of type Type. Possible Types: */
/* digit:		0-9		*/
/* letter:		a-zA-Z_		*/
/* eof:  		code -1		*/
/* layout:		code =< 32	*/
/* punct:		(),;[]{}	*/
/* spec:		<all others>	*/

char_type(C, Type):-
	char_type0(C, Type0), !, Type = Type0.
char_type(_C, symbol).

char_type0(C, digit):-
	C >= 0'0, C =< 0'9.
char_type0(C, letter):-
	C >= 0'a, C =< 0'z ;
	C >= 0'A, C =< 0'Z ;
	C = 0'_.
char_type0(-1, eof).
char_type0(C, layout):-
	C =< 32.
char_type0(0'%, comment).
char_type0(C, punct):-
	member(C, "(),|[]{}").
char_type0(C, solo):-
	member(C, ";!").
char_type0(C, quote):-
	member(C, """'").

/* continued(Type, TypeCont): chars of type TypeCont are allowed in  */
/* tokens starting with a char of type Type */

continued(letter, letter).
continued(letter, digit).
continued(digit, digit).
continued(symbol, symbol).

eot_succ_type(layout).
eot_succ_type(comment).
eot_succ_type(eof).
