:- module(xmlwrite,
	    [xml_format/1, xml_format_noindent/1, xml_format/4,
		xml_format_stream/2, xml_format_stream_noindent/2], []).

:- use_module(library(lists), [reverse/2, append/3, length/2]).
:- use_module(library(format), [format/2]).
%:- use_module(library(format), [format_to_string/3]).
:- use_module(fastformat, [format_to_string/3]).
:- use_module(xmlread, [translate_entities/2]).

xml_format_stream(Stream, Frag) :-
	current_output(CO),
	set_output(Stream),
	( xml_format(Frag)
	-> set_output(CO)
	; set_output(CO),
	    fail
	).

xml_format_stream_noindent(Stream, Frag) :-
	current_output(CO),
	set_output(Stream),
	( xml_format_noindent(Frag)
	-> set_output(CO)
	; set_output(CO),
	    fail
	).

xml_format(Frag) :-
	xml_format(indent, '', [], Frag).

xml_format_noindent(Frag) :-
	xml_format(noindent, '', [], Frag).

xml_format(IndentFlag, NS, PL, Frag) :-
	list(Frag),
	format_fragment(IndentFlag, 0, NS, [], PL, Frag, _).

format_fragment(IndentFlag, _, _,  _,   _,  [],        IndentFlag) :- !.
format_fragment(IndentFlag, I, NS, PPL, PL, [X|Frag1], NextIndent) :-
	format_item(IndentFlag, I, NS, PPL, PL, X, Indent2),
	format_fragment(Indent2, I, NS, PPL, PL, Frag1, NextIndent).

format_item(IndentFlag, I, NS, PPL, PL,
	    env(QName, Attribs, EC), NextIndent) :- !,
	extract_ns(QName, ENS, LN),
	extract_ns_data([], [], '', Attribs, _, EPL0, EA0),
	reverse(EPL0, EPL), reverse(EA0, EA),
	format_item(IndentFlag, I, NS, PPL, PL, e(ENS, LN, EPL, EA, EC),
	    NextIndent).
%
format_item(IndentFlag, I, NS, PPL, PL,
	    e(ENS, LN, EPL, EA, EC0), IndentFlag) :- !,
	(IndentFlag==indent -> fresh_line, indent(I) ; true),
	J is I+1,

	ins_pl(PL, EPL, NPL),

	( (member(Pfx=ENS, EPL) ; member(Pfx=ENS, PL))
	-> format_to_string("~a:~a", [Pfx, LN], Tag), ALS1=ALS2
	; NS=ENS
	-> format_to_string("~a", [LN], Tag), ALS1=ALS2
	; format_to_string("~a", [LN], Tag),
	    format_to_string("~w", [ENS], ENSS),
	    translate_entities(ENSS, ENSST),
	    format_to_string("xmlns=\"~s\"", [ENSST], S1),
	    ALS1=[S1|ALS2]
	),
	format("<~s", [Tag]),
	put_new_suffixes(J, EPL, PPL, ALS2),
	put_attribs(J, EA, NPL, ALS3),
	append(ALS1, ALS3, ALS),
	put_strings(IndentFlag, J, ALS),

	remove_empty_strings(EC0, EC),
	( EC=[], self_closing_tag(Tag) ->
	    format("/>", [])
	;
	    format(">", []),
	    format_fragment(IndentFlag, J, ENS, NPL, PL, EC, Indent2),
	    (Indent2==indent -> fresh_line, indent(I) ; true),
	    format("</~s>", [Tag])
	).
%
format_item(IndentFlag, I, NS, PPL, PL, '$'(LN, Attribs), NextIndent) :- !,
	format_item(IndentFlag, I, NS, PPL, PL,
	    e(NS, LN, PL, Attribs, []), NextIndent).
%
format_item(IndentFlag, J, _, _, _, proci(LANG, S), IndentFlag) :- !,
	(IndentFlag==indent -> fresh_line, indent(J) ; true),
	format("<?~a ~s ?>", [LANG, S]).
%
format_item(_, _, _, _, _, entity(Entity), noindent) :- !,
	( number(Entity)
	-> format("&#~d;", [Entity])
	; format("&~w;", [Entity])
	).
%
format_item(_, _, _, _, _, escaped(S), noindent) :- !,
	putescaped(S).
format_item(_, _, _, _, _, S, noindent) :- string(S),
	putunquoted(S).

% In XHTML, not all tags can be self-closed
% TODO: use atoms for tags
self_closing_tag("a") :- !, fail.
self_closing_tag(_).

extract_ns(NS:LN, NS, LN) :- !.
extract_ns(LN,    '', LN).

extract_ns_data(EPL0, EA0, NS0, [],           NS0, EPL0, EA0) :- !.
extract_ns_data(EPL0, EA0, _,   [xmlns=NS|A], NS1, EPL1, EA1) :- !,
	extract_ns_data(EPL0, EA0, NS, A, NS1, EPL1, EA1).
extract_ns_data(EPL0, EA0, NS0, [xmlns:P=NS|A], NS1, EPL1, EA1) :- !,
	extract_ns_data([P=NS|EPL0], EA0, NS0, A, NS1, EPL1, EA1).
extract_ns_data(EPL0, EA0, NS0, [xmlns:P=NS|A], NS1, EPL1, EA1) :-
	extract_ns_data([P=NS|EPL0], EA0, NS0, A, NS1, EPL1, EA1).
extract_ns_data(EPL0, EA0, NS0, [T|A], NS1, EPL1, EA1) :-
	extract_ns_data(EPL0, [T|EA0], NS0, A, NS1, EPL1, EA1).


ins_pl([],      L, L) :- !.
ins_pl([P=_|A], L, NL) :-
	member(P=_, L), !, ins_pl(A, L, NL).
ins_pl([X|A], L, [X|NL]) :-
	ins_pl(A, L, NL).

put_attribs(_, [],         _,  []) :- !.
put_attribs(J, [NS:A=V|R], PL, [AVS|W]) :-
	member(Pfx=NS, PL), !,
	format_to_string("~w", [V], VS),
	translate_entities(VS, VST),
	format_to_string("~a:~a=\"~s\"", [Pfx, A, VST], AVS),
	put_attribs(J, R, PL, W).
put_attribs(J, [A=V|R], PL, [AVS|W]) :-
	atom(A), !,
	format_to_string("~w", [V], VS),
	translate_entities(VS, VST),
	format_to_string("~a=\"~s\"", [A, VST], AVS),
	put_attribs(J, R, PL, W).
put_attribs(J, [_|R], PL, W) :-
	put_attribs(J, R, PL, W).

put_new_suffixes(_, [],        _,   []).
put_new_suffixes(J, [P=NS|PL], PPL, W) :-
	( member(P=NS, PPL)
	-> W=U
	; format_to_string("~w", [NS], NSS),
	    translate_entities(NSS, NSST),
	    format_to_string("xmlns:~a=\"~s\"", [P, NSST], PNST),
	    W= [PNST|U]
	),
	put_new_suffixes(J, PL, PPL, U).


indent(I) :-
	J is 4*I, tab(J).

/*
putquoted(S) :-
	put_code(0'"),
	putunquoted(S),
	put_code(0'").
*/

%putunquoted(S) :-
%	translate_entities(S, S1), format("~s", [S1]).
putunquoted(S) :-
	format("~s", [S]).
putescaped(S) :-
	translate_entities(S, S1), format("~s", [S1]).

put_strings(_,          _, []).
put_strings(IndentFlag, J, [S|Rest]) :-
	length(S, N),
	format(" ~s", [S]),
	put_strings_1(IndentFlag, J, N, Rest).

put_strings_1(_,      _, _, []) :- !.
put_strings_1(indent, J, N, [S|R]) :- !,
	length(S, M),
	( (N > 40 ; N+M+1 > 40)
	-> fresh_line, indent(J), N1=M
	; N1 is N+M+1
	),
	format(" ~s", [S]),
	put_strings_1(indent, J, N1, R).
put_strings_1(IndentFlags, J, N, [S|R]) :-
	length(S, M),
	N1 is N+M+1,
	format(" ~s", [S]),
	put_strings_1(IndentFlags, J, N1, R).

remove_empty_strings([],     []) :- !.
remove_empty_strings([[]|R], S) :- !,
	remove_empty_strings(R, S).
remove_empty_strings([E|R], [E|S]) :-
	remove_empty_strings(R, S).

fresh_line:-
	format("~N", []).
