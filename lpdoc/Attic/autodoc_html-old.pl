:- module(autodoc_html, [], []).
% (Nothing is exported, because everything works using hooks)

:- use_package([assertions, regtypes]).

:- use_module(lpdocsrc(src(autodoc))).
:- use_module(lpdocsrc(src(autodoc_doctree))).
:- use_module(library(lists), [list_concat/2]).
:- use_module(lpdocsrc(src(comments)), [stringcommand/1]).

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- pred autodoc_rw_command_hook(Format, DocSt, Command, NewCommand)
	: backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(html, DocSt, Command, NewCommand) :- !,
	rw_command(Command, DocSt, NewCommand).

% ......................................................................

rw_command(sp(NS), _, raw(NewCommand)) :- !,
	number_codes(N, NS),
	N1 is N+1,
	html_blank_lines(N1, NewCommand).
rw_command(p(""),                _, raw("<p>")) :- !.
rw_command(noindent(""),         _, nop) :- !.
rw_command(newblock(""),         _, [raw("<br/>")]) :- !. % TODO: remove? just for bibrefs
rw_command(env_('itemize', X),     _, [raw("<UL>"), raw_nl, X, raw("</UL>"), raw_nl]) :- !.
rw_command(env_('enumerate', X),   _, [raw("<OL>"), raw_nl, X, raw("</OL>"), raw_nl]) :- !.
rw_command(env_('description', X), _, [raw("<DL>"), raw_nl, X, raw("</DL>"), raw_nl]) :- !.
rw_command(env_('cartouche', X), _, [raw_nl, raw("<TABLE CELLPADDING=3 BORDER=""1""><TR><TD ALIGN=""CENTER"">"), raw_nl, X, raw_nl, raw("</TD></TR></TABLE>"), raw_nl]) :- !.
rw_command(env_('alert', X), _, [raw_nl, raw("<TABLE CELLPADDING=3 BORDER=""1""><TR><TD ALIGN=""CENTER"">"), raw_nl, X, raw_nl, raw("</TD></TR></TABLE>"), raw_nl]) :- !.
rw_command(env_('verbatim', X),     _, [raw("<pre>"), X, raw("</pre>"), raw_nl]) :- !.
rw_command(item(S), _,       raw("<LI>")) :- doctree_is_empty(S), !.
rw_command(item(S),  _DocSt, NewCommand) :- !,
	NewCommand = [raw("<DT>"), S, raw("<dd>")].
rw_command(footnote(Text), _DocSt, NBody) :- !,
	NBody = [raw("<P><B>Note:</B> "), Text, raw("<P>")].
rw_command('}',                   _, raw("}")) :- !.
rw_command('{',                   _, raw("{")) :- !.
rw_command('@',                   _, raw("@")) :- !.
rw_command(today(""),             _, raw("<date>")) :- !.
rw_command(hfill(""),             _, raw("")) :- !.
rw_command('`'([X]),              _, raw("&"||([X|"grave;"]))) :- !.
rw_command(''''([X]),             _, raw("&"||([X|"acute;"]))) :- !.
rw_command('^'([X]),              _, raw("&"||([X|"circ;"]))) :- !.
rw_command('..'([X]),             _, raw("&"||([X|"uml;"]))) :-	!.
rw_command('"'([X]),              _, raw("&"||([X|"uml;"]))) :-	!.
rw_command('~'([X]),              _, raw("&"||([X|"tilde;"]))) :- !.
rw_command('='([X]),              _, raw([X])) :- !.
rw_command('.'([X]),              _, raw([X])) :- !.
rw_command('u'([X]),              _, raw([X])) :- !.
rw_command('v'([X]),              _, raw([X])) :- !.
rw_command('H'([X]),              _, raw([X])) :- !.
rw_command('t'([X, Y]),           _, raw([X, Y])) :- !.
rw_command('c'([X]),              _, raw([X])) :- !.
rw_command('d'([X]),              _, raw([X])) :- !.
rw_command('b'([X]),              _, raw([X])) :- !.
rw_command('oe'(""),              _, raw("oe")) :- !.
rw_command('OE'(""),              _, raw("OE")) :- !.
rw_command('ae'(""),              _, raw("ae")) :- !.
rw_command('AE'(""),              _, raw("AE")) :- !.
rw_command('aa'(""),              _, raw("&acirc;")) :- !.
rw_command('AA'(""),              _, raw("&Acirc;")) :- !.
rw_command('o'(""),               _, raw("o")) :- !.
rw_command('O'(""),               _, raw("O")) :- !.
rw_command('l'(""),               _, raw("l")) :- !.
rw_command('L'(""),               _, raw("L")) :- !.
rw_command('ss'(""),              _, raw("ss")) :- !.
rw_command('?'(""),               _, raw("?")) :- !.
rw_command('!'(""),               _, raw("!")) :- !.
rw_command('i'(""),               _, raw("i")) :- !.
rw_command('j'(""),               _, raw("j")) :- !.
rw_command(copyright(""),         _, raw("(c)")) :- !.
rw_command(iso(""), _, raw("<B>[<FONT COLOR=""#FF0000"">ISO</FONT>]</B>")) :- !.
rw_command(bullet(""), _,       raw("*")) :- !.
rw_command(result(""), _,       raw("=>")) :- !.
rw_command(uref(URL), _DocSt, NBody) :- !,
	NBody = [raw("<A HREF="""), raw(URL), raw(""">"), raw(URL), raw("</A>")].
rw_command(uref(Text, URL), _DocSt, NBody) :- !,
	NBody = [raw("<A HREF="""), raw(URL), raw(""">"), Text, raw("</A>")].
rw_command(email(Address), _DocSt, NBody) :- !,
	NBody = [raw("<A HREF=""mailto:"), Address, raw(""">&lt;"), Address, raw("&gt;</A>")].
rw_command(email(Text, Address), _DocSt, NBody) :- !,
	NBody = [raw("<A HREF=""mailto:"), Address, raw(""">"), Text, raw("</A>")].
rw_command(image0(IFile), _DocSt, NBody) :- !,
	NBody = [raw("<IMG SRC="""), raw(IFile), raw(".jpg"">")].
rw_command(image0(IFile, Width, Height), _DocSt, NBody) :- !,
	NBody = [raw("<IMG SRC="""), raw(IFile), raw(".jpg"" WIDTH="), raw(Width),
		 raw(" HEIGHT="), raw(Height), raw(">")].
%% Commands with a more or less direct translation to an html command
rw_command(Command, _DocSt, NewAll) :-
	rw_command_body(Command, NewCommand, RBody),
	!,
	NewAll = [raw("<"), raw(NewCommand), raw(">"), RBody, raw("</"), raw(NewCommand), raw(">")].
% .......... (icmd) ..........
rw_command(idx_anchor(_, _, _, _, R0), _, R) :- !, R = R0.
rw_command(simple_link(_,_,_), _, nop) :- !.
rw_command(X, _DocSt, _R) :- !,
	throw(cannot_rewrite(rw_command(X))).

rw_command_body(bf(Body),    "b",          Body) :- !.
rw_command_body(em(Body),    "i",          Body) :- !.
rw_command_body(tt(Body),    "tt",         Body) :- !.
rw_command_body(key(Body),   "tt",         Body) :- !.
rw_command_body(var(Body),   "i",          Body) :- !.
rw_command_body(missing_link(Text), "i", R) :- !, R = raw(Text).
rw_command_body(ref_link(_, Text), "i", R) :- !, R = raw(Text).

html_blank_lines(0, "") :- !.
html_blank_lines(N, [0'<, 0'B, 0'R, 0'>, 0' |R]) :-
	N1 is N-1,
	html_blank_lines(N1, R).

