:- module(lexutils, [], [dcg]).

:- use_module(library(lists), [reverse/2]).

:- export(spaces/2).
spaces --> [X], {space(X)}, !, spaces.
spaces --> [].

:- export(space/1).
space(0' ).
space(0'\t).
space(0'\r).
space(0'\n).

:- export(blank_or_tab/1).
blank_or_tab(X) :-
	member(X, " \t").

:- export(line_break/2).
line_break -->
	( "\n" -> []
	; "\r\n" -> []
	; "\r" ).

:- export(any_blanks_or_tabs/2).
any_blanks_or_tabs -->
	[X], {blank_or_tab(X)}, !, any_blanks_or_tabs.
any_blanks_or_tabs --> [].

:- export(non_substantial_whitespace/2).
non_substantial_whitespace -->
	any_blanks_or_tabs,
	line_break,
	any_blanks_or_tabs,
	non_substantial_whitespace_1.

non_substantial_whitespace_1-->
	any_blanks_or_tabs,
	line_break, !.
non_substantial_whitespace_1--> [].

:- export(trim_spaces/2).
trim_spaces(A, TA) :-
	atom(A),
	atom_codes(A, S),
	trim_spaces(S, TS),
	atom_codes(TA, TS).
trim_spaces(S, TS) :-
	string(S),
	spaces(S, S1),
	reverse(S1, S2),
	spaces(S2, S3),
	reverse(S3, TS).

:- export(lowercase/1).
lowercase(X) :- X >= 0'a, X =< 0'z.
:- export(uppercase/1).
uppercase(X) :- X >= 0'A, X =< 0'Z.
:- export(letter/1).
letter(X) :- lowercase(X) ; uppercase(X).
:- export(digit/1).
digit(X) :-  X >= 0'0, X =< 0'9.

:- export(namestartchar/1).
namestartchar(X) :- letter(X) ; X = 0'_.
:- export(namerestchar/1).
namerestchar(X) :- letter(X) ; digit(X) ; member(X, "_-$.").

:- export(ncname/3).
ncname(NCName) -->
	ncname_1(Codes), {atom_codes(NCName, Codes)}.

ncname_1([X|Rest]) -->
	[X], {namestartchar(X)},
	ncnamerest_1(Rest).
ncnamerest_1([X|Rest]) -->
	[X], {namerestchar(X)}, !,
	ncnamerest_1(Rest).
ncnamerest_1([]) --> [].

:- export(qname/4).
qname(Prefix, NCName) -->
	ncname(Prefix), ":", !, ncname(NCName).
qname('', NCName) -->
	ncname(NCName).

:- export(qname_expand/3).
qname_expand(PL, QName, ExpQN) :-
	qname_expand('', PL, QName, ExpQN).

:- export(qname_expand/4).
qname_expand(DefNS, PL, Prefix:Name, QN) :-
	qname_expand_1(DefNS, PL, Prefix, Name, QN).
qname_expand(DefNS, PL, A, QN) :- atom(A), atom_codes(A, S),
	qname_expand(DefNS, PL, S, QN).
qname_expand(DefNS, PL, S, QN) :- string(S), spaces(S, S1),
	qname(Prefix, Name, S1, S2), spaces(S2, []),
	qname_expand_1(DefNS, PL, Prefix, Name, QN).

qname_expand_1(DefNS, _,  '',     Name, DefNS:Name) :- !.
qname_expand_1(_,     PL, Prefix, Name, NS:Name) :-
	member(Prefix=NS, PL), !.
qname_expand_1(_, _, Prefix, Name, Prefix:Name).

% ----------------------------------------------------------------------

:- export(check_empty/2).
check_empty([], []).

:- export(lookahead/3).
lookahead([],    In,     In).
lookahead([X|L], [X|R1], [X|R2]) :-
	lookahead(L, R1, R2).
