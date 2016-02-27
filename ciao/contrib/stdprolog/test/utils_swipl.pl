% This file defines the following utility predicates in SWI-Prolog

:- set_prolog_flag(gc, false). % !! otherwise we get segfault with gc=true
% vvv turn it on from the command line
:- set_prolog_flag(iso, true).
:- set_prolog_flag(autoload, false). % !! how does this affect...?

:- user:asserta(( message_hook(Exc, Severity, _Lines) :-
   ( Severity=warning -> Code=3 ; Severity=error, Code=4 ), !,
   user:retractall(message_hook(_,_,_)),
   print_message(Severity, Exc),  halt(Code) )).


:- use_module(library(lists)).

% !! document: utils

/*
retractall(X)
numbervars(T, N, NR)
member(X, L)
memberchk(X, L)
append(L1, L2, L3)
select(X, L1, L2)
prefix(L1, L2)
last(L, X) :-
term_variables(T, V)
variant(T1, T2)
between(A, B, C)
*/

% Already built-in present in swiprolog:
%   rectractall/1, member/2, memberchk/2, append/3, select/3, last/2, 

%retractall(X) :-
%	'database:retractall'(X).

%member(X, L) :-
%	'lists:member'(X, L).

%memberchk(X, L) :-
%	'lists:memberchk'(X, L).

%append(L1, L2, L3) :-
%	'lists:append'(L1, L2, L3).

%select(X, L1, L2) :-
%	'lists:select'(X, L1, L2).

%last(L, X) :-
%	'lists:last'(L, X).

% ---

prefix([], _Xs).
prefix([A|As], [A|Bs]) :- prefix(As, Bs).

% ---

%- module(term, [(=)/2, (\=)/2, (==)/2, (\==)/2, (@<)/2, (@=<)/2,
%	 (@>)/2, (@>=)/2, functor/3, arg/3, (=..)/2, char_code/2,
%	 atom_codes/2, number_codes/2, 
%	 atom_chars/2, number_chars/2, atom_length/2, atom_concat/3, 
%	 sub_atom/5, unify_with_occurs_check/2]).
%
%- use_module(err, [err_check/2]).
%- use_module(execution, [not/1]).
%- use_module(lists, [member/2, append/3, length/2]).


/*
% ----------------
X = X.

% ----------------

X \= X :-
	!, fail.
_ \= _.

% ----------------
compare(R, Term1, Term2) :-
	typeof(Term1, Type1),
	typeof(Term2, Type2),
	compare_typed(Type1, Type2, Term1, Term2, R).

typeof(Term, 0) :- var(Term), !.
typeof(Term, 1) :- float(Term), !.
typeof(Term, 2) :- integer(Term), !.
typeof(Term, 3) :- atom(Term), !.
typeof(_, 4).

compare_typed(Type, Type, Term1, Term2, R) :-
	!,
	compare_values(Type, Term1, Term2, R).
compare_typed(Type1, Type2, _, _, R) :-
	compare_num(Type1, Type2, R).

compare_values(0, V1, V2, R) :-
	'$var_index'(V1, I1),
	'$var_index'(V2, I2),
	compare_num(I1, I2, R).
compare_values(1, F1, F2, R) :-
	compare_num(F1, F2, R).
compare_values(2, I1, I2, R) :-
	compare_num(I1, I2, R).
compare_values(3, A1, A2, R) :-
	atom_codes(A1, L1),
	atom_codes(A2, L2),
	compare_intlist(L1, L2, R).
compare_values(4, S1, S2, R) :-
	get_functor(S1, F1, A1),
	get_functor(S2, F2, A2),
	compare_num(A1, A2, AR),
	compare_arity(AR, F1, F2, S1, S2, R).

compare_arity((=), F1, F2, S1, S2, R):-
	!,
	atom_codes(F1, L1),
	atom_codes(F2, L2),
	compare_intlist(L1, L2, FR),
	compare_functor(FR, S1, S2, R).
compare_arity(R, _, _, _, _, R).
	
compare_functor((=), S1, S2, R):-
	!,
	S1 =.. [_|Args1],
	S2 =.. [_|Args2],
	compare_args((=), Args1, Args2, R).
compare_functor(R, _, _, R).

compare_args((=), [A1|Args1], [A2|Args2], R):-
	!,
	compare(R1, A1, A2),
	compare_args(R1, Args1, Args2, R).
compare_args(R, _, _, R).

compare_intlist([], [], R) :- !, R = (=).
compare_intlist([], _, R) :- !, R = (<).
compare_intlist(_, [], R) :- !, R = (>).
compare_intlist([X|L1], [X|L2], R) :-
	!,
	compare_intlist(L1, L2, R).
compare_intlist([X1|_], [X2|_], R) :-
	compare_num(X1, X2, R).

compare_num(N, N, R):-
	!,
	R = (=).
compare_num(N1, N2, R):-
	N1 < N2, !,
	R = (<).
compare_num(_, _, (>)).
 
% ----------------
T1 == T2 :-
	var(T1), !,
	var(T2),
	'$var_index'(T1, I), 
	'$var_index'(T2, I).
T1 == T2 :-
	atomic(T1), !,
	nonvar(T2),
	T1 = T2.
T1 == T2 :-
	nonvar(T2),
	get_functor(T1, Name, Arity),
	get_functor(T2, Name, Arity),
	args_eq(Arity, T1, T2).

args_eq(0, _, _) :- !.
args_eq(AC, T1, T2) :-
	'$arg'(AC, T1, A1),
	'$arg'(AC, T2, A2),
	A1 == A2,
	AC1 is AC - 1,
	args_eq(AC1, T1, T2).

T1 \== T2 :-
	\+(T1 == T2).
	
% ----------------
T1 @< T2 :-
	compare((<), T1, T2).

T1 @=< T2 :-
	\+(compare((>), T1, T2)).

T1 @> T2 :-
	compare((>), T1, T2).

T1 @>= T2 :-
	\+(compare((<), T1, T2)).

% ----------------
get_functor(Term, Funct, Arity) :-
	compound(Term), !,
	'$get_functor'(Term, Funct, Arity).
get_functor(Term, Term, 0).

set_functor(Term, Funct, Arity) :-
	'$set_functor'(Funct, Arity, Term), !.
set_functor(Term, Term, Arity) :-
	atomic(Term),
	integer(Arity),
	Arity = 0, !.
	
functor(Term, Funct, Arity) :-
	nonvar(Term), !,
	get_functor(Term, Funct, Arity).
functor(Term, Funct, Arity) :-
	set_functor(Term, Funct, Arity), !.
functor(Term, Funct, Arity) :-
	err_check(functor(Term, Funct, Arity),
		   [inst(Funct),
		    inst(Arity),
		    atomic(Funct),
		    integer(Arity),
		    nonneg(Arity),
		    max_arity(Arity),
		    if(Arity > 0, atom(Funct))
		   ]).
	
% ----------------
arg(N, Term, Arg) :-
	'$arg'(N, Term, Arg), !.
arg(N, Term, Arg) :-
	'$get_bb'('$error', true), !,
	err_check(arg(N, Term, Arg), 
		   [inst(N),
		    inst(Term),
		    integer(N),
		    nonneg(N),
		    compound(Term)
		   ]).

% ----------------
length_det([], 0).
length_det([_|L], Len):-
	nonvar(L),
	length_det(L, Len0),
	Len is Len0 + 1.

get_args(T, _, 0, Args):-
	!, Args = [].
get_args(T, ArgCtr, ArgNum, [Arg|Args]):-
	'$arg'(ArgCtr, T, Arg),
	ArgCtr1 is ArgCtr + 1,
	ArgNum1 is ArgNum - 1,
	get_args(T, ArgCtr1, ArgNum1, Args).

T =.. [Funct|Args] :-
	nonvar(T), !, 
	get_functor(T, Funct, ArgNum),
	get_args(T, 1, ArgNum, Args).
T =.. [Funct|Args] :-
	nonvar(Args),
	length_det(Args, ArgNum),
	set_functor(T, Funct, ArgNum), !,
	get_args(T, 1, ArgNum, Args).
T =.. List :-
	err_check(T =.. List,
		   [inst(List),
		    list(List),
		    instlist(List),
		    nonemptylist(List),
		    do(List = [Funct|Args]),
		    inst(Funct),
		    do(length(Args, Arity)),
		    if(Arity = 0, atomic(Funct)),
		    if(Arity > 0, atom(Funct)),
		    max_arity(Arity)
		   ]).

% ----------------
char_code(Char, Code) :-
	var(Code),
	'$atom_codes'(Char, [Code]), !.
char_code(Char, Code) :-
	var(Char),
	'$atom_codes'(Char, [Code]), !.
char_code(Char, Code) :-
	'$atom_codes'(Char, [_]), 
	'$atom_codes'(Char0, [Code]), !,
	Char = Char0.
char_code(Char, Code) :-
	err_check(char_code(Char, Code), 
	          [character(Char),
		   inst(Code),
		   integer(Code),
		   char_code(Code)]).

% ----------------
chars_codes([], []).
chars_codes([Ch|Chars], [C|Codes]):-
	'$atom_codes'(Ch, [C]),
	chars_codes(Chars, Codes).

chars_to_codes([], []).
chars_to_codes([Ch|Chars], [C|Codes]):-
	nonvar(Chars),
	'$atom_codes'(Ch, [C]),
	chars_to_codes(Chars, Codes).

% ----------------
atom_codes(Atom, Codes) :-
	var(Atom), 
	'$atom_codes'(Atom, Codes), !.
atom_codes(Atom, Codes) :-
	atom(Atom), !,
	'$atom_codes'(Atom, Codes).
atom_codes(Atom, Codes) :-
	err_check(atom_codes(Atom, Codes), 
	          [atom(Atom),
		   inst(Codes),
		   list(Codes),
		   instlist(Codes),
		   instlistelements(Codes),
		   char_code_list(Codes)]).

% ----------------
atom_chars(Atom, Chars) :-
	var(Atom),
	nonvar(Chars),
	chars_to_codes(Chars, Codes), !,
	'$atom_codes'(Atom, Codes).
atom_chars(Atom, Chars) :-
	atom(Atom), !,
	'$atom_codes'(Atom, Codes),
	chars_codes(Chars, Codes).
atom_chars(Atom, Chars) :-
	err_check(atom_chars(Atom, Chars),
	          [atom(Atom),
		   inst(Chars),
		   list(Chars),
		   instlist(Chars),
		   instlistelements(Chars),
		   char_list(Chars)]).

% ----------------

number_codes(Number, Codes) :-
	check_codes(Codes, IsList),
	( number(Number) ; var(Number), IsList = yes ), !,
	(   IsList = yes -> parse_number(Codes, Number)
        ;   '$number_codes'(Number, Codes)
        ).
number_codes(Number, Codes) :-
	err_check(number_codes(Number, Codes),
	          [number(Number),
		  inst(Codes),
		  list(Codes),
		  instlist(Codes),
		  instlistelements(Codes),
		  char_code_list(Codes)]).

% ----------------
number_chars(Number, Chars) :-
	check_chars(Chars, Codes, IsList),
	( number(Number) ; var(Number), IsList = yes ), !,
	(   IsList = yes -> parse_number(Codes, Number)
        ;   '$number_codes'(Number, Codes), chars_codes(Chars, Codes)
        ).
number_chars(Number, Chars) :-
	err_check(number_chars(Number, Chars),
	          [number(Number),
		   inst(Chars),
		   list(Chars),
		   instlist(Chars),
		   instlistelements(Chars),
		   char_list(Chars)]).

% ----------------
check_codes(Codes, no) :-
	var(Codes), !.
check_codes([], yes) :- !.
check_codes([C|L], no) :-
	var(C), !,
	check_codes(L, _).
check_codes([C|L], IsList) :-
	integer(C), C > 0, C < 256,
	check_codes(L, IsList).
	
% ----------------
check_chars(Chars, _, no) :-
	var(Chars), !.
check_chars([], [], yes) :- !.
check_chars([C|L], _, no) :-
	var(C), !,
	check_chars(L, _, _).
check_chars([C|L], [CC|LC], IsList) :-
	'$atom_codes'(C, [CC]), 
	check_chars(L, LC, IsList).

% ----------------
parse_number(L, N) :-
	try_parse_number(L, N1), !,
	N = N1.
parse_number(L, N) :-
	atom_codes(Atom, L),
	atom_concat('Bad number: ', Atom, Syntax),
	throw(error(syntax_error(Syntax), number_codes(N, L))).

% ----------------
try_parse_number([X|L], N) :-
	is_layout(X), !, 
	try_parse_number(L, N).
try_parse_number([0'-|L], N) :- !,
	parse_poz_number(L, N1),
	(   N1 >= 0 -> N is -N1
        ;   N1 is 1 << 30 -> N = N1  % special case of 'MIN_INT'
        ).
try_parse_number(L, N) :-
	parse_poz_number(L, N), 
	N >= 0.

% ----------------
parse_poz_number([X|L], N) :-
	is_layout(X), !,
	parse_poz_number(L, N).
parse_poz_number([0'0,C|L], N) :-
	member(C, "'box"), !,
	parse_special_number(C, L, N).
parse_poz_number([C|L], N) :-
	digit_based(10, C, _),
	'$number_codes'(N, [C|L]).

% ----------------
parse_special_number(0'\', L, N) :-
	parse_quoted_char(L, N),
	N >=0, N < 256.
parse_special_number(0'b, L, N) :-
	parse_based_number(2, L, N).
parse_special_number(0'o, L, N) :-
	parse_based_number(8, L, N).
parse_special_number(0'x, L, N) :-
	parse_based_number(16, L, N).

% ----------------
parse_quoted_char([0'\'|L], N) :- !,
	L = [0'\'],
	N = 0'\'.
parse_quoted_char([0'\\|L], N) :- !,
	L = [C|L1],
	parse_escape(C, L1, N).
parse_quoted_char([C], C).

% ----------------
parse_escape(C, L, N) :-
	digit_based(8, C, N0), !,
	parse_based(L, 8, N0, N, [0'\\]).
parse_escape(0'x, [C|L], N) :- !,
	digit_based(16, C, N0),
	parse_based(L, 16, N0, N, [0'\\]).
parse_escape(C, [], N) :-
	escape_equiv(C, N).

% ----------------
escape_equiv(0'a, 0'\a).
escape_equiv(0'b, 0'\b).
escape_equiv(0'r, 0'\r).
escape_equiv(0'f, 0'\f).
escape_equiv(0't, 0'\t).
escape_equiv(0'n, 0'\n).
escape_equiv(0'v, 0'\v).
escape_equiv(0'\\, 0'\\).
escape_equiv(0'\', 0'\').
escape_equiv(0'", 0'").
escape_equiv(0'`, 0'`).

% ----------------
parse_based_number(B, [C|L], N) :-
	digit_based(B, C, N0),
	parse_based(L, B, N0, N, []).

% ----------------
parse_based([C|L], B, N0, N, Rest) :-
	digit_based(B, C, V), !,
	add_digit_based(B, N0, V, N1),
	parse_based(L, B, N1, N, Rest).
parse_based(Rest, _, N, N, Rest).

% ----------------
add_digit_based(B, N0, V, N) :-
	catch(N is N0*B+V, error(evaluation_error(int_overflow), _), fail), !.
add_digit_based(B, N0, V, N) :- % special case of 'MIN_INT'
	catch(N is -N0*B-V, error(evaluation_error(int_overflow), _), fail).
	
% ----------------
digit_based(B, C, V) :-
	C >= 0'0, C < 0'0+B, !,
	V is C - 0'0.
digit_based(16, C, V) :-
	C >= 0'a, C =< 0'f, !,
	V is C - 0'a + 10.
digit_based(16, C, V) :-
	C >= 0'A, C =< 0'F, !,
	V is C - 0'A + 10.


% ----------------
is_layout(X) :-
	X =< 32.

% ----------------
atom_length(A, N) :-
	atom(A),
	( var(N) ; integer(N), N >= 0), !,
	'$atom_codes'(A, L),
	length(L, N).
atom_length(A, N) :-
	err_check(atom_length(A, N),
	          [inst(A),
		   atom(A),
		   integer(N),
		   nonneg(N)]).

% ----------------
atom_concat(A1, A2, AC) :-
	atom(AC), 
	( var(A1) ; atom(A1) ),
	( var(A2) ; atom(A2) ), !,
	atom_codes(AC, LC),
	append(L1, L2, LC),
	atom_codes(A1, L1),
	atom_codes(A2, L2).
atom_concat(A1, A2, AC) :-
	var(AC), atom(A1), atom(A2), !,
	atom_codes(A1, L1),
	atom_codes(A2, L2),	
	append(L1, L2, LC),
	atom_codes(AC, LC).
atom_concat(A1, A2, AC) :-
	err_check(atom_concat(A1, A2, AC),
	          [atom(AC),
		   atom(A1),
		   atom(A2),
		   if(var(AC), inst(A1)),
		   if(var(AC), inst(A2))]).

% ----------------
sub_atom(Atom, Before, Length, After, Subatom) :-
	atom(Atom),
	( var(Subatom) ; atom(Subatom) ),
	is_length(Before),
	is_length(Length),
	is_length(After), !,
	atom_codes(Atom, List),
	append(L1, LA, List),
	append(L2, L3, LA),
	length(L1, Before),
	length(L2, Length),
	length(L3, After),
	atom_codes(Subatom, L2).
sub_atom(Atom, Before, Length, After, Subatom) :-
	err_check(sub_atom(Atom, Before, Length, After, Subatom),
	          [inst(Atom),
		   atom(Atom),
		   integer(Before),
		   nonneg(Before),
		   integer(Length),
		   nonneg(Length),
		   integer(After),
		   nonneg(After),
		   atom(Subatom)]).
	
is_length(X) :-	var(X).
is_length(X) :- integer(X), X >= 0.

% ----------------
occurs_check(Term, Var) :-
	var(Term), !,
	Term \== Var.
occurs_check(Term, Var) :-
	get_functor(Term, _, Arity),
	occurs_check(Arity, Term, Var).
 
occurs_check(0, _, _) :- !.
occurs_check(N, Term, Var) :-
	'$arg'(N, Term, Arg),
	occurs_check(Arg, Var),
	M is N-1, !,
	occurs_check(M, Term, Var).
 
unify_with_occurs_check(X, Y) :-
	var(X), var(Y),
	!,
	X = Y.          %  want unify(X,X)
unify_with_occurs_check(X, Y) :-
	var(X),
	!,
	occurs_check(Y, X),             %  X is not in Y
	X = Y.
unify_with_occurs_check(X, Y) :-
	var(Y),
	!,
	occurs_check(X, Y),             %  Y is not in X
	X = Y.
unify_with_occurs_check(X, Y) :-
	atomic(X),
	!,
	X = Y.
unify_with_occurs_check(X, Y) :-
	get_functor(X, F, N),
	get_functor(Y, F, N),
	unify_with_occurs_check(N, X, Y).
	
unify_with_occurs_check(0, X, Y) :- !.
unify_with_occurs_check(N, X, Y) :-
	'$arg'(N, X, Xn),
	'$arg'(N, Y, Yn),
	unify_with_occurs_check(Xn, Yn),
	M is N-1, !,
	unify_with_occurs_check(M, X, Y).

% ----------------
expand_term(T1, T2) :-
	var(T1), !,
	T1 = T2.
expand_term(-->(Head,Body), T2) :-
	!,
	'dcg:dcg rule'(-->(Head, Body), T2).
expand_term(T, T).
*/

% ----------------
%%%% pts %%%% swi_prolog 5.4.7 has numbervars/3
/*
numbervars_arg(0, _, _, N, N) :- !.
numbervars_arg(AN, AC, T, N, NR) :-
	arg(AC, T, Arg), % pts: swiprolog:arg is smarter than aprolog:$arg
	numbervars(Arg, N, N1),
	AC1 is AC + 1,
	AN1 is AN - 1,
	numbervars_arg(AN1, AC1, T, N1, NR).

numbervars(T, N, NR) :-
	var(T), !,
	T = '$VAR'(N), % pts: works in swi-prolog
	NR is N + 1.
numbervars(T, N, NR) :-
	% get_functor(T, _, Arity),
	( compound(T) -> T =.. Ts, length(Ts, TL), Arity is TL-1
	; Arity = 0
	),
	numbervars_arg(Arity, 1, T, N, NR).
*/

% ----------------
%%%% pts %%%% swi_prolog 5.4.7 has term_variables/2, 5.2.13 doesn't

myterm_variables(T, V) :-
	collect_vars(T, V, []).

%% collect_vars(Term, Vs, Vs0): Vs is the list of variables in Term prepended
%%      to Vs0.
collect_vars(V, Vs, Vs0) :-
	var(V),
	\+(var_memberchk(V, Vs0)), !,
	Vs = [V|Vs0].
collect_vars(C, Vs, Vs0) :-
	compound(C), !,
	C =.. [_|Args],
	collect_vars0(Args, Vs, Vs0).
collect_vars(_, Vs, Vs).

%% collect_vars0(List, Vs, Vs0): Vs is the list of variables in all of the
%%      terms in List prepended to Vs0.
collect_vars0([], Vs, Vs).
collect_vars0([H|T], Vs, Vs0) :-
	collect_vars(H, Vs1, Vs0),
	collect_vars0(T, Vs, Vs1).

var_memberchk(V0, [V|_]) :-
	V0 == V, !.
var_memberchk(V0, [_|L]) :-
	var_memberchk(V0, L).

% vvv %%%% pts %%%% swi-prolog has between/3 built-in, and doesn't allow
%     overriding it (!! test case for built-in predicate overrides)
:- ( predicate_property(term_variables(_,_),built_in) -> true
   ; retractall(term_variables/2), asserta((term_variables(A,B) :- myterm_variables(A,B)))
   ).


% ----------------
% vvv !! does SWI-Prolog have it?
subsumes_chk(General, Specific) :-
	\+  (   numbervars(Specific, 0, _),
		\+ General = Specific
	    ).

variant(A, B) :-
	subsumes_chk(A, B),
	subsumes_chk(B, A).

%%%% pts %%%%
mybetween(N, _, N).
mybetween(N0, M, N) :-
	N0 < M, N1 is N0+1, mybetween(N1, M, N).

% vvv %%%% pts %%%% swi-prolog has between/3 built-in, and doesn't allow
%     overriding it (!! test case for built-in predicate overrides)
:- ( predicate_property(between(_,_,_),built_in) -> true
   ; retractall(between/3), asserta((between(A,B,C) :- mybetween(A,B,C)))
   ).
