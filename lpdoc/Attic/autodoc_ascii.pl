:- module(autodoc_ascii, [], []).
% (Nothing is exported, because everything works using hooks)

:- use_package([assertions, regtypes]).

:- use_module(lpdocsrc(src(autodoc))).
:- use_module(lpdocsrc(src(autodoc_doctree))).
:- use_module(lpdocsrc(src(autodoc_images))).
:- use_module(lpdocsrc(src(autodoc_aux)), [ascii_blank_lines/2]).
:- use_module(lpdocsrc(src(comments)), [stringcommand/1]).
:- use_module(library(lists), [list_concat/2]).

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- prop autodoc_rw_command_hook(Backend, DocSt, Command, NewCommand)
	: backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(ascii, DocSt, Command, NewCommand) :- !,
	rw_command(Command, DocSt, NewCommand).

rw_command(sp(NS), _, raw(NewCommand)) :- !,
	number_codes(N, NS),
	N1 is N+1,
	ascii_blank_lines(N1, NewCommand).
rw_command(p(""),                _,    [raw_nl, raw_nl]) :- !.
rw_command(noindent(""),         _,    []) :- !.
rw_command(mathenv(S),           _,    [raw(S)]) :- !. % TODO: Not supported
rw_command(mathenv(_, S),        _,    [raw(S)]) :- !. % TODO: Not supported
rw_command(defmathcmd_(_,_,_),   _,    []) :- !. % TODO: Not supported
rw_command(newblock(""),         _,    [raw_nl]) :- !. % TODO: remove? just for bibrefs
rw_command(env_('itemize', X),     _,  [raw_nl, raw_nl, X, raw_nl, raw_nl]) :- !.
rw_command(env_('enumerate', X),   _,  [raw_nl, raw_nl, X, raw_nl, raw_nl]) :- !.
rw_command(env_('description', X), _,  [raw_nl, raw_nl, X, raw_nl, raw_nl]) :- !.
rw_command(env_('cartouche', X),   _,  [raw_nl, raw("-----------"), raw_nl, X, raw_nl, raw("-----------"), raw_nl]) :- !.
rw_command(env_('alert', X),   _,  [raw_nl, raw("-----------"), raw_nl, X, raw_nl, raw("-----------"), raw_nl]) :- !.
rw_command(env_('verbatim', X),     _, [raw_nl, raw_nl, X, raw_nl, raw_nl]) :- !.
rw_command(item(S),             _,     [raw_nl, raw(" - ")]) :- doctree_is_empty(S), !.
rw_command(item(S),              _DocSt, NewAll) :- !,
	NewAll = [raw_nl, raw(" - "), S, raw(": ")].
rw_command(footnote(Text), _DocSt, NBody) :- !,
	NBody = [raw_nl, raw_nl, raw("Note: "), Text, raw_nl, raw_nl].
rw_command('}',                   _,       raw("}")) :- !.
rw_command('{',                   _,       raw("{")) :- !.
rw_command('@',                   _,       raw("@")) :- !.
% TODO: dummy date inserted, fix
rw_command(today(""),             _,       raw("<date>")) :- !.
rw_command(hfill(""),             _,       []) :- !.
rw_command('`'([X]),              _,       raw([0'`, X])) :- !.
rw_command(''''([X]),             _,       raw([0'", X])) :- !.
rw_command('^'([X]),              _,       raw([0'^, X])) :- !.
rw_command('..'([X]),             _,       raw(['..', X])) :- !.
rw_command('~'([X]),              _,       raw([0'~, X])) :- !.
rw_command('='([X]),              _,       raw([X])) :- !.
rw_command('.'([X]),              _,       raw([X])) :- !.
rw_command('u'([X]),              _,       raw([0':, X])) :- !.
rw_command('v'([X]),              _,       raw([0'v, X])) :- !.
rw_command('H'([X]),              _,       raw([X])) :- !.
rw_command('t'([X, Y]),           _,       raw([X, Y])) :- !.
rw_command('c'([X]),              _,       raw([0',, X])) :- !.
rw_command('d'([X]),              _,       raw([X])) :- !.
rw_command('b'([X]),              _,       raw([X])) :- !.
rw_command('oe'(""),              _,       raw("oe")) :- !.
rw_command('OE'(""),              _,       raw("OE")) :- !.
rw_command('ae'(""),              _,       raw("ae")) :- !.
rw_command('AE'(""),              _,       raw("AE")) :- !.
rw_command('aa'(""),              _,       raw("aa")) :- !.
rw_command('AA'(""),              _,       raw("AA")) :- !.
rw_command('o'(""),               _,       raw("o")) :- !.
rw_command('O'(""),               _,       raw("O")) :- !.
rw_command('l'(""),               _,       raw("l")) :- !.
rw_command('L'(""),               _,       raw("L")) :- !.
rw_command('ss'(""),              _,       raw("ss")) :- !.
rw_command('?'(""),               _,       raw("?")) :- !.
rw_command('!'(""),               _,       raw("!")) :- !.
rw_command('i'(""),               _,       raw("i")) :- !.
rw_command('j'(""),               _,       raw("j")) :- !.
rw_command(copyright(""),         _,       raw("(c)")) :- !.
rw_command(iso(""),               _,       raw("[*ISO*]")) :- !.
rw_command(bullet(""),            _,       raw("*")) :- !.
rw_command(result(""),            _,       raw("=>")) :- !.
rw_command(bf(Body),              _,       Body) :- !.
rw_command(em(Body),              _,       Body) :- !.
rw_command(tt(Body),              _,       Body) :- !.
rw_command(var(Body),             _,       Body) :- !.
rw_command(ref_link(_, Text),     _,       R) :- !,
	R = raw(Text).
rw_command(missing_link(Text),    _,       R) :- !,
	R = raw(Text).
rw_command(uref(URL),            _DocSt, raw(URL)) :- !.
rw_command(uref(Text, URL), _DocSt, NBody) :- !,
	NBody = [Text, raw(" ("), raw(URL), raw(")")].
rw_command(email(Address), _DocSt, Address) :- !.
rw_command(email(Text, Address), _DocSt, NBody) :- !,
	NBody = [Text, raw(" ("), Address, raw(")")].
rw_command(image_auto(IFile, _), DocSt, NBody) :- !,
	locate_and_convert_image(IFile, ['txt'], DocSt, IFile2),
	NBody = [raw("[Image: "), raw(IFile2), raw("]")].
% .......... (icmd) ..........
rw_command(section_env(SecProps, _SectLabel, TitleR, Body), _DocSt, R) :- !,
	fmt_structuring(SecProps, TitleR, SectR),
	R = [SectR, Body].
rw_command(bibitem(Label,_Ref), _DocSt, R) :- !,
	R = [item(bf([string_esc("["), string_esc(Label), string_esc("]")]))].
rw_command(idx_anchor(_, _, _, _, R0), _, R) :- !, R = R0.
rw_command(simple_link(_,_,_,_), _, nop) :- !.
rw_command(X, _DocSt, _R) :- !,
	throw(cannot_rewrite(rw_command(X))).

% TODO: It can be improved
fmt_structuring(SecProps, TitleR, R) :-
	( section_prop(level(Level), SecProps) ->
	    ( Level = 1 -> R0 = "****"
	    ; Level = 2 -> R0 = "***"
	    ; Level = 3 -> R0 = "**"
	    )
	; throw(missing_level_prop(SecProps))
	),
	R = [raw_nl, raw_nl, raw(R0), TitleR, raw_nl].

% ===========================================================================

:- multifile autodoc_finish/2.
autodoc_finish(ascii, _).