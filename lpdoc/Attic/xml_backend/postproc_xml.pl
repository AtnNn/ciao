:- module(postproc_xml, [], [contextual]).

:- use_module(lexutils, [spaces/2, space/1, lookahead/3, 
		check_empty/2]).
:- use_module(library(lists), [append/3, length/2]).
%:- use_module(library(format), [format_to_string/3]).
:- use_module(fastformat, [format_to_string/3]).
:- use_module(library(aggregates), [findall/3]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Content Post-Processing
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- push_prolog_flag(unused_pred_warnings, no).
:- def_context 'PEnv' =
	{env(
		{settings('settings')}, % Various settings
		{index('index')}, % Concept index
		{footnotes('footnotes')}, % A list of footnotes
		{sections('sections')}, % A list of sections
		{secnums('secnum')}, % Current section numbering
		{current_sid('sid')}, % Last generated ID
		{toc('toc')} % TOC list
	    )}.
:- pop_prolog_flag(unused_pred_warnings).
% ----------------------------------------------------------------------

:- export(default_env/1).
default_env / -'PEnv' :-
	make__PEnv([
		settings= [],
		index= [],
		footnotes= [],
		sections= [],
		secnum= (0 -0),
		sid= 0,
		toc= []
	    ]) / -'PEnv'.

% ----------------------------------------------------------------------
/*
default(X, Y) :- (nonvar(X), ! ; X=Y).
inc(X, Y) :- Y is X+1.
pop(X) /source :- [X] /source.
push(X, A, [X|A]).
*/
:- export(prefix/3).
prefix([],    R,      R).
prefix([A|L], [A|R1], R2) :- prefix(L, R1, R2).

% ----------------------------------------------------------------------

% Check for the end of command, and eat an extra space, if it is there
(##) /source :-
	( letter(_) /source -> fail
	; space_or_tab(_) /source -> true
	; true
	).

% Check for the end of command
(#?) /source :-
	( letter(_) /source -> fail
	; true
	).

% Eat extra space if it is there
(#-) /source :-
	( space_or_tab(_) /source -> true
	; true
	).

alpha(C) :- (C>=0'A, C=<0'Z), !.
alpha(C) :- (C>=0'a, C=<0'z), !.

letter(C) /source :- [C] /source, alpha(C).

letters([C|L]) /source :- letter(C) /source, !, letters(L) /source.
letters([]) /source :- true.

cmdname([C|R]) /source :- letter(C) /source, letters(R) /source.

% ----------------------------------------------------------------------

format_to_atom(Format, Args, Atom) :-
	format_to_string(Format, Args, S),
	atom_codes(Atom, S).

% ----------------------------------------------------------------------

:- def_context 'CState' = (source, 'PEnv', content).

% ......................................................................
/*
postproc_content(Frag) / (+source, 'PEnv') :-
	content(ready) / (+source, -_, 'PEnv', +Frag, -[]).
*/
% TODO: remove cut
:- export(add_postproc_content/5).
:- use_module(library(format), [format/3]).
add_postproc_content / (+source, 'PEnv', content) :-
	content(ready) / (+source, -_, 'PEnv', content), !.

content(_) / 'CState' :-
	check_empty / source, !.
content(ready) / 'CState' :-
	[C] / source, space(C), !,
	content(ready) / 'CState'.
content(ready) / 'CState' :-
	paragraph(start, Cont) / (source, 'PEnv', Paragraph-[]), !,
	( Paragraph\=[]
	-> [env(p, [], Paragraph)] / content
	; true
	),
	content(Cont) / 'CState'.
content(section) / 'CState' :- !,
	section_cmd / 'CState',
	content(ready) / 'CState'.
content(subsection) / 'CState' :- !,
	subsection_cmd / 'CState',
	content(ready) / 'CState'.
content(image) / 'CState' :- !,
	image_cmd / 'CState',
	content(ready) / 'CState'.
content(verbatim) / 'CState' :- !,
	verbatim_cmd / 'CState',
	content(ready) / 'CState'.
content(code) / 'CState' :- !,
	code_cmd / 'CState',
	content(ready) / 'CState'.
content(list(Type)) / 'CState' :- !,
	list_cmd(Type) / 'CState',
	content(ready) / 'CState'.
% TODO: Is this correct? It does not take any character from source
content(_) / 'CState':-
	true.

% ----------------------------------------------------------------------

:- def_context 'PState' = (source, 'PEnv', para).

paragraph(start, Cont) / 'PState' :-
	[X] / source, space(X), !,
	paragraph(start, Cont) / 'PState'.
paragraph(start, Cont) / 'PState' :-
	"@p"/source, (##)/source, !,
	paragraph(inpara, Cont) / 'PState'.
paragraph(start, Cont) / 'PState' :-
	"@sp"/source, (#?)/source, immediate(_)/source, !,
	paragraph(inpara, Cont) / 'PState'.
paragraph(inpara, Cont) / 'PState' :-
	literal(start, PCont) / (source, 'PEnv', +Lit, -[]), !,
	( Lit\=[] ->
	    [Lit] / para
	; true
	),
	( PCont= content(Cont) -> true
	; paragraph(PCont, Cont) / 'PState'
	).
paragraph(cite, Cont) / 'PState' :- !,
	cite_cmd / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(ref, Cont) / 'PState' :- !,
	ref_cmd / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(uref, Cont) / 'PState' :- !,
	uref_cmd / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(footnote, Cont) / 'PState' :- !,
	footnote_cmd / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(formatting(Type), Cont) / 'PState' :- !,
	formatting_cmd(Type) / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(email, Cont) / 'PState' :- !,
	email_cmd / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(index(Type), Cont) / 'PState' :- !,
	index_cmd(Type) / 'PState',
	paragraph(inpara, Cont) / 'PState'.
paragraph(Current, Cont) / 'PState' :-
	( Current\=inpara
	-> paragraph(inpara, Cont) / 'PState'
	; Cont= ready
	).

% ======================================================================

:- def_context 'LState' = (source, 'PEnv', lit).

literal(_, Cont) / 'LState' :-
	detect_break(Cont)/source, !.
literal(start, Cont) / 'LState' :-
	space_or_tab(_) / source, !,
	" " / lit,
	literal(space, Cont) / 'LState'.
literal(start, Cont) / 'LState' :-
	line_break / source, !,
	" " / lit,
	literal(nl, Cont) / 'LState'.
literal(space, Cont) / 'LState' :-
	space_or_tab(_) / source, !,
	literal(space, Cont) / 'LState'.
literal(space, Cont) / 'LState' :-
	line_break / source, !,
	literal(nl, Cont) / 'LState'.
literal(nl, Cont) / 'LState' :-
	space_or_tab(_) / source, !,
	literal(nl, Cont) / 'LState'.
literal(nl, content(ready)) / 'LState' :-
	line_break / source, !.
literal(_, Cont) / 'LState' :-
	char_item / 'LState', !,
	literal(start, Cont) / 'LState'.
literal(_, content(ready)) / 'LState' :-
	true.

% ......................................................................

% TODO: If some break is not included here, the parser enters an
% infinite loop (JF)

detect_break(Cont) /source :-
	( lookahead("}")/source -> Cont = content(stop)
	; lookahead("@end")/source -> Cont = content(stop)
	; lookahead("@item")/source -> Cont = content(stop)
	; "@"/source,
	  ( "begin{verbatim}"/source -> Cont = content(verbatim)
	  ; "begin{code}"/source -> Cont = content(code)
	  ; "begin{itemize}"/source -> Cont = content(list(itemize))
	  ; "begin{enumerate}"/source -> Cont = content(list(enumerate))
	  ; "begin{description}"/source -> Cont = content(list(description))
	  ; "section"/source, (#?)/source -> Cont = content(section)
	  ; "subsection"/source, (#?)/source -> Cont = content(subsection)
	  ; "image"/source, (#?)/source -> Cont = content(image)
	  ; "p"/source, (##)/source -> Cont = content(ready)
	  ; "noindent"/source, (#?)/source -> Cont = content(ready) % TODO: Check that it is ok
	  ; "sp"/source, (#?)/source -> immediate(_)/source, Cont = content(ready)
	  ; "bf"/source, (#?)/source -> Cont = formatting(bf)
	  ; "em"/source, (#?)/source -> Cont = formatting(em)
	  ; "tt"/source, (#?)/source -> Cont = formatting(tt)
	  ; "key"/source, (#?)/source -> Cont = formatting(key)
	  ; "var"/source, (#?)/source -> Cont = formatting(var)
	  ; "code"/source, (#?)/source -> Cont = formatting(code)
	  ; "cite"/source, (#?)/source -> Cont = cite
	  ; "ref"/source, (#?)/source -> Cont = ref
	  ; "uref"/source, (#?)/source -> Cont = uref
	  ; "footnote"/source, (#?)/source -> Cont = footnote
	  ; "email"/source, (#?)/source -> Cont = email
	  ; "index"/source, (#?)/source -> Cont = index(index)
	  ; "cindex"/source, (#?)/source -> Cont = index(cindex)
	  ; "apl"/source, (#?)/source -> Cont = index(apl)
	  ; "concept"/source, (#?)/source -> Cont = index(concept)
	  ; "decl"/source, (#?)/source -> Cont = index(decl)
	  ; "lib"/source, (#?)/source -> Cont = index(lib)
	  ; "op"/source, (#?)/source -> Cont = index(op)
	  ; "file"/source, (#?)/source -> Cont = index(file)
	  ; "pred"/source, (#?)/source -> Cont = index(pred)
	  )
	).

% ......................................................................

char_item / 'LState' :-
	( "@"/source ->
	    ( char_cmd / (source, lit) -> true
	    ; true
	    )
	; "``"/source -> "&ldquo;" /lit
	; "''"/source -> "&rdquo;" /lit
	; lookahead("{") / source ->
	    immediate(Arg) / source,
	    prefix(Arg) / lit
	; [X] / source ->
	    % note: not processed source elements must be escaped for the backend
	    translate_entities([X], X2), % TODO: improve
%	    format(user_error, "b:~s~na:~s~n", [Lit, Lit1]),
	    prefix(X2) / lit
	), !.

:- use_module(xmlread, [translate_entities/2]).

% (preceded by @)
char_cmd / (source, lit) :-
	( special_symbol / (source, lit)
	; special_char / (source, lit)
	; accented_char / (source, lit)
	; comment_cmd / source
	), !.

% ----------------------------------------------------------------------

% If the content consists of a single paragraph, make content equal to 
% content of the paragraph.  Otherwise, keep as is. 

simplify_content([env(p, _, L)|A], R) :-
	( A=[env(p, _, _)|_] -> fail
	; append(L, A, R)
	), !.
simplify_content(C, C).

:- export(simplify_content/2).

% ----------------------------------------------------------------------

complex_immediate(X) / (source, 'PEnv') :-
	argument(A) / source,
	add_postproc_content / (+A, 'PEnv', +X, -[]).

immediate(X) / source :-
	argument(A) / source,
	flatten(A, X).

argument(A) / source :-
	( check_empty / source -> A= ""
	; [C] / source, space(C) -> A= ""
	; lookahead([C]) / source, [C] \= "{" -> A= ""
	; "{" / source -> argument1 / (source, +A, -[])
	).

argument1 / (source, arg) :-
	( check_empty / source
	; "}" / source
	), !.
argument1 / (source, arg) :-
	( "@"/source, [C]/source  -> "@"/arg, [C]/arg
	; "{" / source ->
	    "{" / arg,
	    argument1 / (source, +Y, -[]),
	    prefix(Y) / arg,
	    "}" / arg
	; [C] / source -> [C] / arg
	),
	!, argument1 / (source, arg).

flatten1 / (arg, flat) :-
	check_empty / arg, !.
flatten1 / (arg, flat) :-
	( "@"/arg ->
	    ( char_cmd / (arg, flat) -> true
	    ; cmdname(Cmd) / arg,
	      ( " " / arg -> prefix(Cmd) / flat
	      ; true
	      )
	    )
	; "{" / arg -> argument1 / (arg, +Sub, -[]),
	    flatten1 / (+Sub, -[], flat),
	    " " / flat
	; [C] / arg -> [C] / flat
	), !,
	flatten1 / (arg, flat).

:- export(flatten/2).
flatten(Xs, Ys) / (*) :-
	flatten1 / (+Xs, -_, +Ys, -[]).

% ......................................................................

space_or_tab(32) /source :- " " /source.
space_or_tab(9) /source :- "\t" /source.

any_spaces_or_tabs/source :-
	( space_or_tab(_) /source -> any_spaces_or_tabs/source
	; true
	).

line_break/source :-
	( "\r\n" /source -> true
	; "\n" /source -> true
	; "\r" /source
	).

% ----------------------------------------------------------------------

% (preceded by @)
special_symbol /(source, str) :-
	( "copyright" /source, (##) /source -> "&#169; " /str
	; "result" /source, (##) /source -> " => " /str
	; "bullet" /source, (##) /source -> "&#186;" /str
	; "iso" /source, (##) /source -> "<span class=\"iso\">ISO</span>" /str
	; "today" /source, (##) /source -> "TODAY " /str % TODO: insert real date
	; "hfill" /source, (##) /source -> true % just ignore horizontal filling
	).

% ......................................................................

% (preceded by @)
special_char /(source, str) :-
	( "j" /source, (##) /source -> "j" /str % not in ISO-8859-1
	; "i" /source, (##) /source -> "i" /str % not in ISO-8859-1
	; "!" /source, (##) /source -> "&#161;" /str
	; "?" /source, (##) /source -> "&#191;" /str
	; "ss" /source, (##) /source -> "&#223;" /str
	; "L" /source, (##) /source -> "L" /str % not in ISO-8859-1
	; "l" /source, (##) /source -> "l" /str % not in ISO-8859-1
	; "O" /source, (##) /source -> "&#216;" /str
	; "o" /source, (##) /source -> "&#248;" /str
	; "AA" /source, (##) /source -> "&#197;" /str
	; "aa" /source, (##) /source -> "&#229;" /str
	; "AE" /source, (##) /source -> "&#198;" /str
	; "ae" /source, (##) /source -> "&#230;" /str
	; "OE" /source, (##) /source -> "OE" /str % not in ISO-8859-1
	; "oe" /source, (##) /source -> "oe" /str % not in ISO-8859-1
	; "{" /source, (#-) /source -> "{" /str
	; "}" /source, (#-) /source -> "}" /str
	; "@" /source, (#-) /source -> "@" /str
	; empty_or_space/source -> " " /str
	).

empty_or_space/source :-
	(check_empty/source ; [C] /source, space(C)).

% ......................................................................

% (preceded by @)
accented_char / (source, str) :-
	( "`" /source, (#?) /source -> Acc = grave
	; "'" /source, (#?) /source -> Acc = acute
	; "^" /source, (#?) /source -> Acc = circ
	; ".." /source, (#?) /source -> Acc = uml
	; "~" /source, (#?) /source -> Acc = tilde
	; "=" /source, (#?) /source -> Acc = long % don't know exactly
	; "." /source, (#?) /source -> Acc = dotabove % don't know exactly
	; "u" /source, (#?) /source -> Acc = inflex % don't know exactly
	; "v" /source, (#?) /source -> Acc = caron % don't know exactly
	; "H" /source, (#?) /source -> Acc = dacute % don't know exactly
	; "t" /source, (#?) /source -> Acc = trans % don't know exactly
	; "c" /source, (#?) /source -> Acc = cedil
	; "d" /source, (#?) /source -> Acc = dot % don't know exactly
	; "b" /source, (#?) /source -> Acc = below % don't know exactly
	),
	immediate(Text) / source,
	( Text = [Char], accented_code(Char, Acc, Code) ->
	    format_to_string("&#~d;", [Code], Entity),
	    prefix(Entity) / str
	; prefix(Text) / str
	).

accented_code(0'A, grave, 192).
accented_code(0'A, acute, 193).
accented_code(0'A, circ, 194).
accented_code(0'A, tilde, 195).
accented_code(0'A, ml, 196).
accented_code(0'A, ing, 197).
accented_code(0'C, cedil, 199).
accented_code(0'E, grave, 200).
accented_code(0'E, acute, 201).
accented_code(0'E, irc, 202).
accented_code(0'E, uml, 203).
accented_code(0'I, grave, 204).
accented_code(0'I, acute, 205).
accented_code(0'I, irc, 206).
accented_code(0'I, uml, 207).
accented_code(0'N, tilde, 209).
accented_code(0'O, grave, 210).
accented_code(0'O, acute, 211).
accented_code(0'O, irc, 212).
accented_code(0'O, tilde, 213).
accented_code(0'O, ml, 214).
accented_code(0'U, grave, 217).
accented_code(0'U, acute, 218).
accented_code(0'U, irc, 219).
accented_code(0'U, uml, 220).
accented_code(0'Y, acute, 221).
accented_code(0'a, grave, 224).
accented_code(0'a, acute, 225).
accented_code(0'a, irc, 226).
accented_code(0'a, tilde, 227).
accented_code(0'a, ml, 228).
accented_code(0'a, ing, 229).
accented_code(0'c, cedil, 231).
accented_code(0'e, grave, 232).
accented_code(0'e, acute, 233).
accented_code(0'e, irc, 234).
accented_code(0'e, uml, 235).
accented_code(0'i, grave, 236).
accented_code(0'i, acute, 237).
accented_code(0'i, irc, 238).
accented_code(0'i, uml, 239).
accented_code(0'n, tilde, 241).
accented_code(0'o, grave, 242).
accented_code(0'o, acute, 243).
accented_code(0'o, irc, 244).
accented_code(0'o, tilde, 245).
accented_code(0'o, ml, 246).
accented_code(0'u, grave, 249).
accented_code(0'u, acute, 250).
accented_code(0'u, irc, 251).
accented_code(0'u, uml, 252).
accented_code(0'y, acute, 253).

% ......................................................................

% (preceded by @)
comment_cmd/source :-
	"comment" /source, (#?) /source, argument(_) /source.

% ----------------------------------------------------------------------

formatting_cmd(Type) / 'PState' :-
	( Type=bf -> Elem= b, Attribs= []
	; Type=em -> Elem= i, Attribs= []
	; Type=tt -> Elem= tt, Attribs= []
	; Type=key -> Elem= span, Attribs= [class=doc_key]
	; Type=var -> Elem= var, Attribs= []
	; Type=code -> Elem= code, Attribs= []
	),
	argument(Arg) / source,
	paragraph(start, _) / (+Arg, -_, 'PEnv', +Para, -[]), !,
	[env(Elem, Attribs, Para)] / para.

% ......................................................................

email_cmd / 'PState' :-
	immediate(A1) / source,
	( lookahead("{") / source ->
	    immediate(A2) / source,
	    format_to_string("mailto:~s", [A2], HRefS)
	; format_to_string("mailto:~s", [A1], HRefS)
	),
	atom_codes(HRef, HRefS),
	[env(a, [href=HRef, class=doc_email], [A1])] / para.

% ......................................................................

uref_cmd / 'PState' :-
	immediate(A1) / source,
	( lookahead("{") / source ->
	    immediate(A2) / source,
	    HRefS= A2
	; HRefS= A1
	),
	atom_codes(HRef, HRefS),
	[env(a, [href=HRef], [A1])] / para.

% ......................................................................

footnote_cmd / 'PState' :-
	argument(FootnoteText) / source,
	get(FL) / footnotes,
	length(FL, N),
	FID is N+1,
	append(FL, [FID=FootnoteText], FL1),
	set(FL1) / footnotes,
        %
	format_to_string("[~d]", [FID], Marker),
	format_to_atom("#footnote_no_~d", [FID], FOOTFRAG),
	format_to_atom("footnote_ref_~d", [FID], REFFRAG),
	[env(a,
		[name=REFFRAG, href=FOOTFRAG,
		    class=doc_footref], [Marker])] / para.

% ......................................................................

ref_cmd / 'PState' :-
	immediate(Title) / source,
	get(Sections) / sections,
	( member(Title=SID, Sections) ->
	    true
	; title_hash(Title, SID) / 'PEnv',
	  set([Title=SID|Sections]) / sections
	),
	format_to_string("#~w", [SID], SIDFRAGS),
	atom_codes(SIDFRAG, SIDFRAGS),
	format_to_string("[&#167;&#160;~s]", [Title], Marker),
	[env(a, [href=SIDFRAG, class=doc_ref], [Marker])] / para.

% ......................................................................

section_cmd / (source, 'PEnv', content) :-
	immediate(Title) / source,
	get(Sections) / sections,
	( member(Title=SID, Sections) ->
	    true
	; title_hash(Title, SID) / 'PEnv',
	  set([Title=SID|Sections]) / sections
	),
	get(S0-_) / secnum,
	S1 is S0+1,
	set(S1-0) / secnum,
	get(TOC0) / toc,
	current_file(File) / +'PEnv',
	format_to_atom("~s#~w", [File, SID], TOCREF),
	append(TOC0, [entry(S1, TOCREF, Title)], TOC),
	set(TOC) / toc,
	format_to_string("~d ~s", [S1, Title], Marker),
	[env(a, [name=SID], []), env(h3, [], [Marker])] / content.

% ......................................................................

:- export(current_file/2).
current_file(File) / +'PEnv' :-
	get(Settings) / settings,
	( member(currentfile=File, Settings), !
	; File= ""
	).
% ......................................................................

subsection_cmd / (source, 'PEnv', content) :-
	immediate(Title) / source,
	get(Sections) / sections,
	( member(Title=SID, Sections)
	-> true
	; title_hash(Title, SID) / 'PEnv',
	  set([Title=SID|Sections]) / sections
	),
	get(S-SS0) / secnum,
	SS1 is SS0+1,
	set(S-SS1) / secnum,
	get(TOC0) / toc,
	current_file(File) / +'PEnv',
	format_to_atom("~s#~w", [File, SID], TOCREF),
	append(TOC0, [entry(S-SS1, TOCREF, Title)], TOC),
	set(TOC) / toc,
	format_to_string("~d.~d ~s", [S, SS1, Title], Marker),
	[env(a, [name=SID], []), env(h4, [], [Marker])] / content.

% ......................................................................

title_hash(_, SID) / 'PEnv' :-
	local_anchor(SID) / 'PEnv'.

local_anchor(SID) / 'PEnv' :-
	get(SID0) / sid,
	SID is SID0+1,
	set(SID) / sid.

% ......................................................................

cite_cmd / 'PState' :-
	immediate(Ref) / source,
	format_to_string("bibliography.html#~s", [Ref], HRefS),
	atom_codes(HRef, HRefS),
	append("[", Ref, A1), append(A1, "]", A2),
	[env(a, [href=HRef, class=doc_cite], [A2])] / para.

% ......................................................................

image_cmd / 'CState' :-
	immediate(BaseNameS) / source,
	( lookahead("{") / source ->
	    immediate(_) / source,
	    ( lookahead("{") / source ->
	        immediate(_) / source
	    ; true
	    )
	; true
	),
%	append(BaseNameS, ".png", URLS),
	append(BaseNameS, ".jpg", URLS),
	atom_codes(URL, URLS),
	[env(img, [src=URL], [])] / content.

% ----------------------------------------------------------------------

verbatim_cmd / (source, 'PEnv', content) :-
	any_spaces_or_tabs / source,
	collect_verbatim / (source, +Verbatim, -[]),
	[env(pre, [class=doc_verbatim], [escaped(Verbatim)])] / content.

collect_verbatim / (source, str) :-
	check_empty / source, !.
collect_verbatim / (source, str) :-
	line_break/source, any_spaces_or_tabs/source, "@end{verbatim}"/source,
	!.
collect_verbatim / (source, str) :-
	[X] / source, [X] / str,
	collect_verbatim / (source, str).

% ----------------------------------------------------------------------

code_cmd / (source, 'PEnv', content) :-
	any_spaces_or_tabs / source,
	collect_code / (source, +Verbatim, -[]),
	[env(pre, [class=doc_code], [Verbatim])] / content.

collect_code / (source, str) :-
	check_empty / source, !.
collect_code / (source, str) :-
	line_break/source, any_spaces_or_tabs/source, "@end{code}"/source,
	!.
collect_code / (source, str) :-
	[X] / source, [X] / str,
	collect_code / (source, str).

% ......................................................................

list_cmd(Type) / (source, 'PEnv', content) :-
	format_to_string("@end{~a}", [Type], EndMarker),
	collect_list(Type, EndMarker) / (source, 'PEnv', Items-[]),
	translate_list(Type, Items) / content.

translate_list(Type, Items) / content :- list_env_tag(Type, ETag), !,
	% Format as a HTML list 
	list_env_items(Items, ItemsTr),
	[env(ETag, [], ItemsTr)] / content.
translate_list(Type, Items) / content :- desc_env_tag(Type), !,
	% Format as a HTML table
	desc_env_items(Items, Items2),
	[env(dl, [], Items2)] / content.
% translate_list(Type, Items) / content :- table_env_tag(Type), !,
% 	% Format as a HTML table
% 	table_env_items(Items, Rows),
% 	[env(table, [class=doc_desc_table], Rows)] / content.

collect_list(Type, EndMarker) / (source, 'PEnv', items) :-
	spaces / source,
	( check_empty / source -> true
	; prefix(EndMarker) / source -> true
	; "@item"/source, (#?)/source ->
	    complex_immediate(Bullet) / (source, 'PEnv'),
	    content(ready) / (source, 'PEnv', +Frag0, -[]),
	    simplify_content(Frag0, Frag),
	    [Bullet/Frag] / items,
	    collect_list(Type, EndMarker) / (source, 'PEnv', items)
	; content(ready) / (source, 'PEnv', +Frag0, -[]) ->
	    simplify_content(Frag0, Frag),
%	   [""/Frag] / items,
	    collect_list(Type, EndMarker) / (source, 'PEnv', items)
	; true
	).

% TODO: using HTML dl is an alternative
%list_env_tag(description, ul).
list_env_tag(enumerate, ol).
list_env_tag(itemize, ul).

list_env_items([],        []).
list_env_items([[]/I|L], [env(li, [], I)|R]) :- !,
	list_env_items(L, R).
list_env_items([B/I|L],
	    [env(li, [], [env(span, [class=doc_bullet], B)|I])|R]) :- !,
	list_env_items(L, R).

desc_env_tag(description).

desc_env_items([], []).
desc_env_items([B/I|L], [env(dt, [], B), env(dd, [], I)|R]) :-
	desc_env_items(L, R).

% (JF) Not working very well...
%
% table_env_tag(description).
% 
% table_env_items([],        []).
% table_env_items([B/I|L],   [R|Rs]) :- !,
% 	R = env(tr, [],
% 		    [
% 			env(td, [], B),
% 			env(td, [], I)
% 		    ]),
% 	table_env_items(L, Rs).

% list_env_items(Items, ItemsTr) :-
%  	(  any_item_starts_with_p( Items)
%  	-> make_all_items_big( Items, Items2)
%  	;  Items2= Items
%  	),
%  	add_item_prefixes( Items2, ItemsTr).
% 
% any_item_starts_with_p([_/[env(p, _, _)|_]|_]) :- !.
% any_item_starts_with_p([_|L]) :- any_item_starts_with_p(L).
% 
% make_all_items_big([],      []).
% make_all_items_big([B/I|L], [B/I|R]) :-
% 	( I=[env(p, _, _)|_]
% 	; I=[env(pre, _, _)|_]
% 	; I=[env(ul, _, _)|_]
% 	; I=[env(ol, _, _)|_]
% 	), !,
% 	make_all_items_big(L, R).
% make_all_items_big([B/[I0|IR]|L], [B/[env(p, [], [I0])|IR]|R]) :-
% 	make_all_items_big(L, R).

% ======================================================================

:- export(gen_footnotes/4).
gen_footnotes / ('PEnv', content) :-
	get(Footnotes) / footnotes,
	collect_footnotes(Footnotes) / ('PEnv', List-[]),
	( List=[] -> true
	; [env(div, [class=doc_footnotes],
		    [env(h2, [], ["Footnotes"]),
			env(ol, [], List)])] / content
	).

collect_footnotes([]) / ('PEnv', list) :- true.
collect_footnotes([F|L]) / ('PEnv', list) :-
	collect_footnote(F) / ('PEnv', list),
	collect_footnotes(L) / ('PEnv', list).

collect_footnote(FID=Text) / ('PEnv', list) :-
	content(ready) / (+Text, -_, 'PEnv', Frag0-[]),
	simplify_content(Frag0, Frag),
	format_to_atom("#footnote_ref_~d", [FID], REFFRAG),
	format_to_atom("footnote_no_~d",   [FID], FOOTFRAG),
	[env(li, [value=FID],
		[env(a, [name=FOOTFRAG, class=doc_footref,
			    href=REFFRAG], ["[^]"]), " "|Frag])] / list.

% ======================================================================

index_cmd(Type) / 'PState' :-
	immediate(Arg) / source,
	index1_cmd(Type, Arg) / 'PState'.
index1_cmd(concept, Concept) / 'PState' :-
	enter_index(concept, Concept, CID) / 'PEnv',
	[env(a, [name=CID], []),
	 env(span, [class=doc_concept], [Concept])] / para.
index1_cmd(index, Concept) / 'PState' :-
	enter_index(concept, Concept, CID) / 'PEnv',
	[env(a, [name=CID], []),
	 env(span, [class=doc_concept], [Concept])] / para.
index1_cmd(cindex, Concept) / 'PState' :-
	enter_index(concept, Concept, CID) / 'PEnv',
	[env(a, [name=CID], [])] / para.
index1_cmd(lib, Concept) / 'PState' :-
	enter_index(lib, Concept, CID) / 'PEnv',
	[env(a, [name=CID], []),
	    env(tt, [], [Concept])] / para.
index1_cmd(apl, Concept) / 'PState' :-
	enter_index(apl, Concept, CID) / 'PEnv',
	[env(a, [name=CID], []),
	    env(tt, [], [Concept])] / para.
index1_cmd(decl, Concept) / 'PState' :-
	enter_index(decl, Concept, CID) / 'PEnv',
	[env(a, [name=CID], []),
	    env(tt, [], [Concept])] / para.
index1_cmd(file, Concept) / 'PState' :-
	enter_index(file, Concept, CID) / 'PEnv',
	[env(a, [name=CID], []),
	    env(tt, [], [Concept])] / para.
index1_cmd(pred, Concept) / 'PState' :-
	pred_frag(Concept, Frag),
	enter_index(pred, Concept, CID) / 'PEnv',
	[env(a, [name=CID, href=Frag], [
		    env(tt, [], [Concept])])] / para.

enter_index(Type, Title, CID) / 'PEnv' :-
	title_hash(Title, CID) / 'PEnv',
	current_file(File) / +'PEnv',
	format_to_atom("~s#~w", [File, CID], FRAG),
	get(Index) / index,
	atom_codes(FileA, File),
	Entry= entry(Type, FileA, FRAG, Title),
	set([Entry|Index]) / index.

pred_frag(PredNameStr, Frag) :-
	urlenc(PredNameStr, S1, []),
	format_to_atom("#~s", [S1], Frag).

:- export(pred_anchor/2).
pred_anchor(PredNameStr, Anchor) :-
	urlenc(PredNameStr, S1, []),
	atom_codes(Anchor, S1).

urlenc / (+source, result) :-
	( check_empty / source -> true
	; [C] / source,
	  ( C>=0'0, C=<0'9 -> [C] / result
	  ; C>=0'A, C=<127 -> [C] / result
	  ; C<16 ->
	    format_to_string("%0~16R", [C], S1),
	    prefix(S1) / result
	  ; format_to_string("%~16R", [C], S1),
	    prefix(S1) / result
	  ),
	  urlenc / (+source, result)
	).

% ======================================================================

:- export(gen_indices/5).
gen_indices(File) / ('PEnv', content) :-
	gen_index("Concept Index", concept, File) / ('PEnv', content),
	gen_index("Library Index", lib, File) / ('PEnv', content),
	gen_index("Application Index", apl, File) / ('PEnv', content),
	gen_index("Declaration Index", decl, File) / ('PEnv', content),
	gen_index("File Index", file, File) / ('PEnv', content),
	gen_index("Predicate Index", pred, File) / ('PEnv', content).

% ----------------------------------------------------------------------

gen_index(Title, Type, File) / ('PEnv', content) :-
	get(I) / index,
	findall(ID/Text, ( Entry= entry(Type, File, ID, Text),
		member(Entry, I) ), Entries),
	( Entries=[] ->
	    true
	; sort_entries(Entries, SortedEntries),
	  [env(h2, [],                [Title])] / content,
	  [env(ul, [class=doc_index], ListItems)] / content,
	  entries_to_list(SortedEntries) / (ListItems-[])
	).

% ......................................................................

sort_entries(Entries, SortedEntries) / (*) :-
	sort_entries_2(Entries) / ([]-SortedEntries).

sort_entries_2([]) / slist :- true.
sort_entries_2([Entry|L]) / slist :-
	sort_ins(Entry) / slist,
	sort_entries_2(L) / slist.

sort_ins(ID/Text) / slist :-
	get(S) / slist,
	( S = [] ->
	    set([ID/Text]) / slist
	; S = [ID2/Text2|S2],
	  ( Text @=< Text2 ->
	      set([ID/Text|S]) / slist
	  ; sort_ins(ID/Text) / (S2-S2prim),
	    set([ID2/Text2|S2prim]) / slist
	  )
	).

% ......................................................................

entries_to_list([]) / items :- true.
entries_to_list([FRAG/Text|L]) / items :-
	other_frags(Text) / (Frags-[], L-L1),
	[env(li, [], ItemContent)] / items,
	add_entry_parts(Text, [FRAG|Frags]) / (ItemContent-[]),
	entries_to_list(L1) / items.

other_frags(Text) / (frags, list) :-
	( [FRAG/Text] / list ->
	    [FRAG] / frags, other_frags(Text) / (frags, list)
	; true
	).

add_entry_parts(Text, Frags) / ic :-
	( Frags=[FRAG] ->
	    [env(a, [href=FRAG], [Text])] / ic
	; [Text] / ic, add_entry_refs(1, Frags) / ic
	).

add_entry_refs(_, []) / ic :- true.
add_entry_refs(I, [FRAG|L]) / ic :-
	format_to_string("[^~d]", [I], T),
	( I>1
	-> [", "] / ic
	; [" - "] / ic
	),
	[env(a, [href=FRAG], [T])] / ic,
	I1 is I+1,
	add_entry_refs(I1, L) / ic.
