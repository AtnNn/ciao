%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

:- module(write, []).

:- use_module(lists, [memberchk/2]).


/* Private put_code/1 without stream type checking */
put_code(C) :-
	'$put_code'(C).


/*  WARNING!
    This file was written to assist portability and to help people
    get a decent set of output routines off the ground fast.  It is
    not particularly efficient.  Information about atom names and
    properties should be precomputed and fetched as directly as
    possible, and strings should not be created as lists!

    The four output routines differ in the following respects:
    [a] display doesn't use operator information or handle {X} or
	[H|T] specially.  The others do.
    [b] print calls portray/1 to give the user a chance to do
	something different.  The others don't.
    [c] writeq puts quotes around atoms that can't be read back.
	The others don't.
    Since they have such a lot in common, we just pass around a
    single Style argument to say what to do.

    In a Prolog which supports strings;
	write(<string>) should just write the text of the string, this so
	that write("Beware bandersnatch") can be used.  The other output
	commands should quote the string.

    listing(Preds) is supposed to write the predicates out so that they
    can be read back in exactly as they are now, provided the operator
    declarations haven't changed.  So it has to use writeq.  $VAR(X)
    will write the atom X without quotes, this so that you can write
    out a clause in a readable way by binding each input variable to
    its name.
*/

write_style(Term, Style) :-
	write_out(Term, Style, 1200, punct, _).


%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.

maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put_code(Char).
maybe_paren(_, _, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put_code(32).
maybe_space(quote, alpha) :- !,
	put_code(32).
maybe_space(_, _).



%   put_string(S)
%   writes a list of character codes.

put_string([]).
put_string([H|T]) :-
	put_code(H),
	put_string(T).

%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.

put_string([], Q) :-
	put_code(Q).
put_string([Q|T], Q) :- !,
	put_code(Q), put_code(Q),
	put_string(T, Q).
put_string([H|T], Q) :-
	put_code(H),
	put_string(T, Q).



%   write_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.

write_variable(V) :-
	'$var_index'(V, I),
	number_codes(I, S), 
	put_code(0'_),
	put_string(S).


%   write_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style,
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

%   Style is of format : style(Quoted, IgnoreOps, NumberVars)
%   Quoted, IgnoreOps are either 'true' or 'false'
%   %%%% pts %%%% NumberVars is either 'false', 'true' (only int arg) or
%     'atom' (int and atom args)

write_out(Term, _, _, Ci, alpha) :-
	var(Term), !,
	maybe_space(Ci, alpha),
	write_variable(Term).
write_out('$VAR'(N), style(_,_,NumberVars), _, Ci, Co) :-
	NumberVars \== false,
	( integer(N), N >= 0 -> true
	; NumberVars == atom, atom(N) %%%% pts %%%%
	), !,
	( atom(N) -> write_atom(N, style(false, false, false), Ci, Co) %%%% pts %%%%
	; write_VAR(N, Ci, Co)
	).
write_out(N, _, _, Ci, alpha) :-
	number(N), !,
	(   N < 0 -> maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),
	number_codes(N, String),
	put_string(String).
write_out(Atom, Style, Prio, _, punct) :-
	atom(Atom),
	Prio =< 1200,
	current_op(P, _, Atom),
	P > Prio,
	!,
	put_code(40),
	(   Style = (true,_,_) -> write_atom(Atom, Style, punct, _)
	;   atom_codes(Atom, String), put_string(String)
	),
	put_code(41).
write_out(Atom, Style, _, Ci, Co) :-
	atom(Atom),
	!,
	write_atom(Atom, Style, Ci, Co).
write_out(Term, Style, _, Ci, punct) :- 
	Style = style(_,true,_),
	!,
	functor(Term, Fsymbol, Arity),
	write_atom(Fsymbol, Style, Ci, _),
	write_args(0, Arity, Term, 40, Style).
write_out({Term}, Style, _, _, punct) :- !,
	put_code(123),
	write_out(Term, Style, 1200, punct, _),
	put_code(125).
write_out([Head|Tail], Style, _, _, punct) :- !,
	put_code(91),
	write_out(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_out((A,B), Style, Prio, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	maybe_paren(1000, Prio, 40, Ci, C1),
	write_out(A, Style, 999, C1, _),
	put_code(44),
	write_out(B, Style, 1000, punct, C2),
	maybe_paren(1000, Prio, 41, C2, Co).
write_out(Term, Style, Prio, Ci, Co) :-
	functor(Term, F, N),
	write_out(N, F, Term, Style, Prio, Ci, Co).


write_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, fx, F), P is O-1
	;   current_op(O, fy, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	write_atom(F, Style, C1, C2),
	arg(1, Term, A),
	write_out(A, Style, P, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xf, F), P is O-1
	;   current_op(O, yf, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_out(A, Style, P, C1, C2),
	write_atom(F, Style, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_out(2, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xfy, F), P is O-1, Q = O
	;   current_op(O, xfx, F), P is O-1, Q = P
	;   current_op(O, yfx, F), Q is O-1, P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_out(A, Style, P, C1, C2),
	write_oper(F, O, Style, C2, C3),
	arg(2, Term, B),
	write_out(B, Style, Q, C3, C4),
	maybe_paren(O, Prio, 41, C4, Co).
write_out(N, F, Term, Style, _Prio, Ci, punct) :-
	write_atom(F, Style, Ci, _),
	write_args(0, N, Term, 40, Style).


write_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, _Ci, punct) :-
	put_code(32),
	write_atom(Op, Style, punct, _),
	put_code(32).


write_VAR(N, Ci, alpha) :-
	%write(z1),
	maybe_space(Ci, alpha),
	%write(z2),
	Letter is N mod 26 + 65,
	%write(z3(Letter)),
	put_code(Letter),
	%write(z4),
	(   N < 26 -> true %%%% pts %%%% Dat: added `N < 26'
	;   Rest is N//26, number_codes(Rest, String),
	    put_string(String)
	), !.

write_atom(('!'), _, _, punct) :- !,
	put_code(33).
write_atom((';'), _, _, punct) :- !,
	put_code(59).
write_atom([], _, _, punct) :- !,
	put_code(91), put_code(93).
write_atom({}, _, _, punct) :- !,
	put_code(123), put_code(125).
write_atom(Atom, Style, Ci, Co) :-
	atom_codes(Atom, String),
	(   classify_name(String, Co),
	    maybe_space(Ci, Co),
	    put_string(String)
	;   Style = style(true,_,_), Co = quote,
	    maybe_space(Ci, Co),
	    put_code(39), put_string(String, 39)
	;   Co = alpha,
	    put_string(String)
	),  !.

%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in write_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, and just looked up.  This has to
%   be as fast as you can make it.

classify_name([H|T], alpha) :-
	H >= 97, H =< 122,
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).

classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	(  H >= 97, H =< 122
	;  H >= 65, H =< 90
	;  H >= 48, H =< 57
	;  H =:= 95
	), !,
	classify_alpha_tail(T).

classify_other_tail([]).
classify_other_tail([H|T]) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).



%   write_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .

write_args(N, N, _, _, _) :- !,
	put_code(41).
write_args(I, N, Term, C, Style) :-
	put_code(C),
	J is I+1,
	arg(J, Term, A),
	write_out(A, Style, 999, punct, _),
	write_args(J, N, Term, 44, Style).



%   write_tail(Tail, Style)
%   writes the tail of a list of a given style.

write_tail(Var, _) :-			%  |var]
	var(Var),
	!,
	put_code(124),
	write_variable(Var),
	put_code(93).
write_tail([], _) :- !,			%  ]
	put_code(93).
write_tail([Head|Tail], Style) :- !,	%  ,Head tail
	put_code(44),
	write_out(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_tail(Other, Style) :-		%  |junk]
	put_code(124),
	write_out(Other, Style, 999, punct, _),
	put_code(93).



