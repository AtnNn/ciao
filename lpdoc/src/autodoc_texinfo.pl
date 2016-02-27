:- module(autodoc_texinfo, [], [assertions, regtypes, fsyntax]).
% (Nothing is exported, because everything works using hooks)

:- doc(title,"Texinfo Backend").
:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Jose F. Morales").

:- use_module(lpdocsrc(src(autodoc_state))).
:- use_module(lpdocsrc(src(autodoc_filesystem))).
:- use_module(lpdocsrc(src(autodoc_structure))).
:- use_module(lpdocsrc(src(autodoc_index))).
:- use_module(lpdocsrc(src(autodoc_doctree))).
:- use_module(lpdocsrc(src(autodoc_images))).
:- use_module(lpdocsrc(src(autodoc_settings))).
:- use_module(library(lists),      [list_concat/2, append/3]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(format),     [format/3]).
:- use_module(fastformat, [format_to_string/3]).
:- use_module(library(messages)).
:- use_module(lpdocsrc(src(comments)), [stringcommand/1,
		version_descriptor/1]).

% ======================================================================

:- multifile autodoc_escape_string_hook/5.

autodoc_escape_string_hook(texinfo, InputType, NS, _DocSt, VS) :- !,
	( InputType = normal ->
	    texinfo_escape(NS, VS)
	; InputType = verb ->
	    texinfo_escape_verb(NS, VS)
	; NS = VS
	).

% ======================================================================

% Tips for info files (excerpt from info's info):
%
%  * Write the indexing commands that refer to a whole section
%    immediately after the section command, and write the indexing
%    commands that refer to a paragraph before that paragraph.
%
%  * Insert blank like between a sectioning command and the first
%    following sentence or paragraph, or between the indexing commands
%    associated with the sectioning command and the first following
%    sentence or paragraph, as shown in the tip on indexing.
%    Otherwise, a formatter may fold title and paragraph together.
%  
%  * Always insert a blank line before an `@table' command and after
%    an `@end table' command; but never insert a blank line after an
%    `@table' command or before an `@end table' command.
%  
%  * Insert blank lines before and after `@itemize' ... `@end itemize'
%    and `@enumerate' ... `@end enumerate' in the same way.

% Other tips from previous versions of LPdoc:
%
%    "In texinfo, it is important to eliminates any blanks or tabs
%    that appear at the beginning of a line. Although leading blanks
%    are OK for the printed manuals, they produce weird info files."

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- pred autodoc_rw_command_hook(Backend, DocSt, Command, NewCommand)
	: backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(texinfo, DocSt, Command, NewCommand) :- !,
	rw_command(Command, DocSt, NewCommand).

%% Refs to nodes / sections / chapter --some chars not allowed.
rw_command(missing_link(Text), _DocSt, R) :- !,
	% TODO: leave as @ref or as normal text?
	SectLabel = Text, % just in case the reference was not found
	R = [raw("@ref"), raw("{"), raw(SectLabel), raw("}")].
rw_command(ref_link(Link, Text), _DocSt, R) :- !,
	% TODO: Use SectLabel0 or _Text?
	( Link = link_to(_, SectLabel0) ->
	    get_nodename(SectLabel0, SectLabel)
	; SectLabel = Text % just in case the reference was not found
	),
	R = [raw("@ref"), raw("{"), raw(SectLabel), raw("}")].
rw_command(cite_link(_, Text), _DocSt, R) :- !,
	% (already resolved)
 	R = [raw(Text)].
rw_command(sp(NS), _, NewCommand) :- !,
	NewCommand = infocmd("sp", raw(NS)).
rw_command(p(""),        _, [raw_fc, raw_nleb]) :- !.
rw_command(noindent(""), _, infocmd("noindent")) :- !.
rw_command(mathenv(S),          _, [infoenv("tex", [raw("$"), raw(S), raw("$")])]) :- !.
rw_command(mathenv(display,S),  _, [infoenv("tex", [raw("$$"), raw(S), raw("$$"), raw_nl])]) :- !. % this one is new in our texinfo
rw_command(defmathcmd_(Cmd,NS,Def), _, R) :- number_codes(N, NS), !,
	% new math command (MathJax)
	def_cmd_args(1, N, Args),
	R = infoenv("tex", [
              % I must use \gdef (not \def) because @tex regions are
              % processed in a TeX group.
              raw("\\gdef"), Args, raw(Cmd), raw("{"), raw(Def), raw("}")
%              raw("\\newcommand{"), raw(Cmd), raw("}["), raw(N), raw("]{"), raw(Def), raw("}")
            ]).
rw_command(newblock(""), _, [infocmd("*")]) :- !. % TODO: remove? just for bibrefs
rw_command(env_('itemize', X),     _, [infocmd("itemize", raw("@bullet{}")), X, infocmd("end", raw("itemize"))]) :- !.
rw_command(env_('enumerate', X),   _, [infocmd("enumerate", []), X, infocmd("end", raw("enumerate"))]) :- !.
rw_command(env_('description', X), _, [infocmd("table", raw("@asis")), X, infocmd("end", raw("table"))]) :- !.
rw_command(env_('cartouche', X),   _, [infocmd("cartouche", []), X, infocmd("end", raw("cartouche"))]) :- !.
rw_command(env_('alert', X),   _, [infocmd("cartouche", []), X, infocmd("end", raw("cartouche"))]) :- !.
rw_command(env_('verbatim', X),    _, [infocmd("smallexample", []), X, infocmd("end", raw("smallexample"))]) :- !.
rw_command(item(S),           _, R) :- !,
	( doctree_is_empty(S) ->
	    % TODO: in order to use infocmd properly in this case,
	    % item_env should be defined (equivalent to the section
	    % problem) -- JF
	    R = [raw_fc, raw("@item ")]
	; R = infocmd("item", S)
	).
% TODO: is @today here a tex command? 'infocmd' broke pdf alignment
rw_command(today(""),       _, [raw("@today"), raw_nl]) :- !.
%rw_command(today(""),       _, infocmd("today", [])) :- !.
% rw_command(hfill(""),_,[if(tex, infocmd("hfill", []))]) :- !.
rw_command(hfill(""), _, infocmd("hfill", [])) :- !.
rw_command(iso(""),   _, raw("@key{ @bullet{} ISO @bullet{} }")) :- !.
rw_command(uref(URL), _, NBody) :- !,
	NBody = [raw("@uref{"), raw(URL), raw("}")].
rw_command(uref(Text, URL), _DocSt, NBody) :- !,
	NBody = [raw("@uref{"), raw(URL), raw(","), Text, raw("}")].
rw_command(email(Address), _, NBody) :- !,
	NBody = [raw("@email{"), Address, raw("}")].
rw_command(email(Text, Address), _DocSt, NBody) :- !,
	NBody = [raw("@email{"), Address, raw(","), Text, raw("}")].
rw_command(image_auto(IFile0, Opts), DocSt, NBody) :- !,
	locate_and_convert_image(IFile0, ['eps'], DocSt, IFile1),
	( append(IFile2, ".eps", IFile1) -> true % required by texinfo.tex
	; IFile2 = IFile1
	),
	atom_codes(AFile2, IFile2),
	% TODO: Possible bug here: make sure that IFile is a relative
	% file and that images are preserved (for distributing .texi)
	docst_backend(DocSt, Backend),
	absfile_for_aux(AFile2, Backend, AFile),
	atom_codes(AFile, IFile),
	( Opts = [] ->
	    NBody = [raw("@image{"), raw(IFile), raw("}")]
	; Opts = [Width, Height] ->
	    NBody = [raw("@image{"), raw(IFile), raw(","), raw(Width),
	             raw("pt,"), raw(Height), raw("pt}")]
        ).
rw_command('}',                _, raw("@}")) :- !.
rw_command('{',                _, raw("@{")) :- !.
rw_command('@',                _, raw("@@")) :- !.
rw_command('`'([X]), _, raw("@`"||[X])) :- !.
rw_command(''''("i"), _, raw("@\'{@dotless{i}}")) :- !. %% Special case for i
rw_command(''''([X]), _, raw("@\'"||[X])) :- !.
% NOTE: Escaped ^ due to fsyntax!
rw_command(^'^'([X]),  _, raw("@^"||[X])) :- !.
rw_command('..'("i"), _, raw("@\"{@dotless{i}}")) :- !. %% Special case for i
rw_command('..'([X]), _, raw("@\""||[X])) :- !.
rw_command('"'("i"),  _, raw("@\"{@dotless{i}}")) :- !. %% Special case for i
rw_command('"'([X]),  _, raw("@\""||[X])) :- !.
% NOTE: Escaped ~ due to fsyntax!
rw_command(^'~'([X]),  _, raw("@~"||[X])) :- !.
rw_command('='([X]),  _, raw("@="||[X])) :- !.
%% Commands with a more or less direct translation to a texinfo command
rw_command(Command, _DocSt, NewAll) :-
	rw_command_body(Command, NewCommand, Body),
	!,
	NewAll = [raw("@"), raw(NewCommand), raw("{"), Body, raw("}")].
% .......... (icmd) ..........
rw_command(infocmd(Cmd), _, NewCommand) :- !,
	% TODO: This one does not add trailing blank, this is why it
	%       is not equivalent to infocmd(Cmd, []). Is that
	%       necessary for 'info'? --JF
	NewCommand = [raw_fc, raw("@"), raw(Cmd), raw_nleb].
rw_command(infocmd(Cmd, Rest), _, NewCommand) :- !,
	NewCommand = [raw_fc, raw("@"), raw(Cmd), raw(" "), Rest, raw_nleb].
rw_command(infoenv(Cmd, Body), _DocSt, R) :- !,
	R = [infocmd(Cmd),
	     Body,
	     infocmd("end", raw(Cmd))].
rw_command(infoenv(Cmd, Param, Body), _DocSt, R) :- !,
	R = [infocmd(Cmd, Param),
	     Body,
	     infocmd("end", raw(Cmd))].
% an environment that only shows in tex (without text duplication)
% TODO: a direct tex backend would deprecate hacks like this one
rw_command(infoenv_onlytex(Cmd, Body), _DocSt, R) :- !,
	R = [if(tex, infocmd(Cmd)),
	     Body,
	     if(tex, infocmd("end", raw(Cmd)))].
%
rw_command(section_env(SecProps, SectLabel, TitleR, BodyR), DocSt, R) :- !,
	fmt_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, R).
rw_command(backend_include_component(Name), _DocSt, R) :- !,
	atom_codes(Name, NameS),
	R = infocmd("include", raw(NameS)).
rw_command(hfill, _DocSt, R) :- !, % vertical space
	%% some versions of makeinfo do not seem to like @hfill... 
	R = if(tex, infocmd("hfill")).
rw_command(linebreak,        _, [raw_fc, raw_nleb]) :- !.
rw_command(subsection_title(Xs), _DocSt, R) :- !,
	R = infoenv("strong", Xs).
rw_command(twocolumns(X), _DocSt, R) :- !, R = X. % ignore
rw_command(itemize_none(Xs), _DocSt, R) :- !,
	R = infoenv("itemize", Xs).
rw_command(itemize_plain(Xs), _DocSt, R) :- !,
	R = infoenv("itemize", Xs).
rw_command(itemize_minus(Xs), _DocSt, R) :- !,
	R = infoenv("itemize", raw("@minus"), Xs).
rw_command(itemize_bullet(Xs), _DocSt, R) :- !,
	R = infoenv("itemize", raw("@bullet"), Xs).
rw_command(description_env(Xs), _DocSt, R) :- !,
	% TODO: strong moved to itemize
	R = infoenv("table", raw("@asis"), Xs).
%	R = infoenv("table", Xs).
rw_command(cartouche(X), _DocSt, R) :- !,
	R = infoenv("cartouche", X).
rw_command(optional_cartouche(X), _DocSt, R) :- !,
	% (a cartouche only in tex, with some internal padding)
	R = [if(tex, infocmd("vfill")),
	     infoenv_onlytex("cartouche", [
	       if(tex, sp("1")),
	       X,
	       if(tex, sp("1"))
             ]),
	     if(tex, infocmd("vfill"))
	     ].
rw_command(alert(X), _DocSt, R) :- !,
	R = infoenv("cartouche", X).
rw_command(bibitem(Label,_Ref), _DocSt, R) :- !,
	R = [item(bf([string_esc("["), string_esc(Label), string_esc("]")]))].
rw_command(idx_anchor(Indices, Label, Key, _OutLink, Text), _DocSt, R) :- !,
	R = [backend_idx(Indices, Label, Key), Text].
rw_command(backend_idx(Indices, _, Key), DocSt, R) :- !,
	fmt_backend_idx(Indices, Key, DocSt, R).
rw_command(copyright_page(CopyrightR), _DocSt, R) :-
	R = [backend_comment("Copyright page"),
	     infocmd("page"),
	     infocmd("vskip", raw("0pt plus 1filll")),
	     CopyrightR,
	     raw_nl].
rw_command(cover_title(TitleR, SubtitleR), _DocSt, R) :- !,
	R = [infocmd("title", TitleR),
	     backend_comment("@font@authorrm=cmbx10 scaled @magstep2"),
	     SubtitleR2],
	fmt_cover_subtitle(SubtitleR, SubtitleR2).
rw_command(cover_subtitle_extra(Rs), _DocSt, R) :- !,
        % Use @subtitle info commands to write the extra subtitle lines
        % TODO: right place to put it?
	fmt_cover_subtitle_extra(Rs, R).
rw_command(authors(AuthorRs), _DocSt, R) :- !,
	fmt_authors(AuthorRs, R).
rw_command(backend_comment(String), _DocSt, R) :- !,
	R = infocmd("comment", raw(String)).
rw_command(quotation(X), _DocSt, R) :- !,
	R = infoenv("quotation", X).
rw_command(setpagenumber(N), _DocSt, R) :- !,
	format_to_string("~d", [N], Codes),
	R = infocmd("pageno", raw(Codes)).
rw_command(backend_printindex(IndexId), _DocSt, R) :- !,
	atom_codes(IndexId, IndexIdS),
	R = infocmd("printindex", raw(IndexIdS)).
rw_command(pred_in_toc(PN, Type), _DocSt, R) :-
        % Put the predicate in the table of contents
	% (only in the tex version)
	PN = F/A,
	format_to_string("~w (~w)", [F/A, Type], S),
	R = if(tex, [
              raw_fc, raw("@edef@temp{@noexpand@writetocentry{"),
	      raw("@realbackslash unnumbsubsubsecentry{"),
	      string_esc(S), raw("}}}"), raw_nleb,
              raw_fc, raw("@temp"), raw_nleb]).
rw_command(left_and_right(Left, Right), _DocSt, R) :- !,
	R = [Left, hfill, Right].
rw_command(navigation_env(_, _), _DocSt, R) :- !, R = [].
rw_command(defpred(IdxLabel, Type, Text, PN, Body), DocSt, R) :- !,
	( docst_opt('-shorttoc', DocSt) ->
	    % Do not put the predicates in the table of contents
	    R1 = []
	; R1 = pred_in_toc(PN, Type)
	),
	PN = F/A, format_to_string("~w/~w", [F, A], S),
	idx_get_indices(def, Type, Indices),
	R = [R1,
             backend_idx(Indices, IdxLabel, string_esc(S)),
	     infoenv("deffn", [raw(Text), raw(" "), string_esc(S), raw(":")], Body)].
rw_command(if(Cond, X), _DocSt, R) :- !,
        % TODO: with a real .tex backend some conditional environments could disappear
	( Cond = tex ->
	    R = infoenv("iftex", X)
	; Cond = info ->
	    R = infoenv("ifinfo", X)
	; Cond = notinfo ->
	    R = infoenv("ifnotinfo", X)
	; throw(cannot_rewrite(if(Cond, X)))
	).
rw_command(end_document, _DocSt, R) :- !,
	R = infocmd("bye").
rw_command(simple_link(_,_,_,_), _, nop) :- !.
rw_command(X, _DocSt, _R) :- !,
	throw(cannot_rewrite(rw_command(X))).

rw_command_body(footnote(Body), "footnote", Body) :- !.
rw_command_body(bf(Body),       "strong",   Body) :- !.
rw_command_body(em(Body),       "emph",     Body) :- !.
rw_command_body(tt(Body),       "code",     Body) :- !.
rw_command_body(key(Body),      "key",      Body) :- !.
%% A variable in a program
rw_command_body(var(Body), "code", Body) :- !.
%% Accents, etc.
rw_command_body('.'([X]),      "dotaccent",    raw([X])) :- !.
rw_command_body('u'([X]),      "u",            raw([X])) :- !.
rw_command_body('v'([X]),      "v",            raw([X])) :- !.
rw_command_body('H'([X]),      "H",            raw([X])) :- !.
rw_command_body('t'([X, Y]),   "tieaccent",    raw([X, Y])) :- !.
rw_command_body('c'([X]),      ",",            raw([X])) :- !.
rw_command_body('d'([X]),      "udotaccent",   raw([X])) :- !.
rw_command_body('b'([X]),      "ubaraccent",   raw([X])) :- !.
rw_command_body('oe'(""),      "oe",           raw("")) :- !.
rw_command_body('OE'(""),      "OE",           raw("")) :- !.
rw_command_body('ae'(""),      "ae",           raw("")) :- !.
rw_command_body('AE'(""),      "AE",           raw("")) :- !.
rw_command_body('aa'(""),      "aa",           raw("")) :- !.
rw_command_body('AA'(""),      "AA",           raw("")) :- !.
rw_command_body('o'(""),       "o",            raw("")) :- !.
rw_command_body('O'(""),       "O",            raw("")) :- !.
rw_command_body('l'(""),       "l",            raw("")) :- !.
rw_command_body('L'(""),       "L",            raw("")) :- !.
rw_command_body('ss'(""),      "ss",           raw("")) :- !.
rw_command_body('?'(""),       "questiondown", raw("")) :- !.
rw_command_body('!'(""),       "exclamdown",   raw("")) :- !.
rw_command_body('i'(""),       "dotless",      raw("i")) :- !.
rw_command_body('j'(""),       "dotless",      raw("j")) :- !.
rw_command_body(copyright(""), "copyright",    raw("")) :- !.
rw_command_body(bullet(""),    "bullet",       raw("")) :- !.
rw_command_body(result(""),    "result",       raw("")) :- !.

def_cmd_args(I, N, []) :- I >= N, !.
def_cmd_args(I, N, [X|Xs]) :-
	number_codes(I, IS),
	X = raw("#"||IS),
	I1 is I + 1,
	def_cmd_args(I1, N, Xs).

% TODO: add a table with the list of info commands types, and use that
%       to choose to insert a new line or not. See "@-Command Syntax"
%       section in "Info"'s info.

fmt_section_env(SecProps, TopSectLabel0, TitleR2, RestR, DocSt, ModR) :-
	section_prop(file_top_section, SecProps),
	section_prop(coversec(SubtitleRs,
	                      SubtitleExtraRs,
	                      AuthorRs,
			      AddressRs,
			      GVersShortR,
			      GVersR,
			      CopyrightR), SecProps),
	!,
	( section_prop(paper_opts(StartPage, PaperType), SecProps) -> true ; fail ),
	get_nodename(TopSectLabel0, TopSectLabel),
	% Version (if available)
	( doctree_is_empty(GVersShortR) ->
	    GVersShortRs = []
	; GVersShortRs = [GVersShortR]
	),
	% Address
	( AddressRs = [] ->
	    AddressRs2 = []
	; % reuse the info @author command for this
	  AddressRs2 = authors([string_esc("")|AddressRs])
	),
	% Document skeleton
	fmt_header_and_cover(TitleR2, PaperType, DocSt, HeaderR),
	ModR = [
	  HeaderR,
	  raw_nl,
	  if(tex, [
            raw_nl,
	    infoenv("titlepage", [
	      cover_title(TitleR2, SubtitleRs),
	      cover_subtitle_extra(SubtitleExtraRs), % TODO: format in other way?
	      cover_subtitle_extra(GVersShortRs), % TODO: format in other way?
	      authors(AuthorRs),
	      AddressRs2,
	      copyright_page(CopyrightR)
            ]),
	    raw_nl,
	    setpagenumber(StartPage),
	    % This unfortunately must be before the Summary.
	    % TODO: can I fix this with a direct tex backend?
	    %backend_comment("Contents:"),
	    %backend_comment("@summarycontents"),
	    % This is the table of contents (for tex)
	    infocmd("contents")
          ]),
          if(info, [
	    % These are the SummaryR and CopyrightR which appear as headers
	    % in the info file, but are not seen otherwise.
	    % It actually gives problems because it generates index entries
	    % that are outside any node --summary commented out.
            GVersR,
	    %SummaryR, raw_nl,
	    raw_nl,
	    CopyrightR,
	    raw_nl,
	    % The top node
            infocmd("node", [raw("Top, "), raw(TopSectLabel), raw(",(dir),(dir)")]),
	    infocmd("top", raw(TopSectLabel))
          ]),
	  RestR,
	  end_document
        ].
fmt_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, R) :-
	fmt_section(SecProps, SectLabel, TitleR, BodyR, DocSt, R).

% ---------------------------------------------------------------------------

% Translate to info commands to insert index entries
fmt_backend_idx([], _, _, []).
fmt_backend_idx([IdxName|Ids], KeyR, DocSt, Rs) :-
	( docst_has_index(IdxName, DocSt),
	  typeindex(IdxName, IndexId, _, _, _) ->
	    atom_codes(IndexId, IndexIdS),
	    append(IndexIdS, "index", IdxCmd),
	    Rs = [infocmd(IdxCmd, KeyR)|Rs0]
	; Rs = Rs0
	),
	fmt_backend_idx(Ids, KeyR, DocSt, Rs0).

fmt_authors(AuthorRs, R) :-
	( AuthorRs = [] ->
	    R = infocmd("author") % Nicer front pg than if no auth command
	; map_infocmd(AuthorRs, "author", R)
	).

fmt_cover_subtitle(Rs, Rs2) :-
        apply_emph(Rs, Rs1),
	map_infocmd(Rs1, "subtitle", Rs2).

apply_emph([], []).
apply_emph([R|Rs0], [em(R)|Rs]) :- apply_emph(Rs0, Rs).

fmt_cover_subtitle_extra(Rs, Rs2) :-
	map_infocmd(Rs, "subtitle", Rs2).

% apply info Command to each R
map_infocmd([],     _Command, []).
map_infocmd([R|Rs], Command,  [R2|Rs2]) :-
	R2 = infocmd(Command, R),
	map_infocmd(Rs, Command, Rs2).

fmt_header_and_cover(TitleR, PaperType, DocSt, R) :-
	docst_currmod(DocSt, Name),
	atom_codes(Name, NameS),
	%
	atom_codes(PaperType, PaperTypeS),
	( docst_opt('-onesided', DocSt) ->
	    NewPage = "on"
	; NewPage = "odd"
	),
	all_indices(DocSt, Indices),
	define_indices(Indices, DocSt, InR),
	R = [
          raw("\\raggedbottom"), raw_nl,
	  raw("\\input texinfo @c -*- texinfo -*-"), raw_nl,
	  backend_comment("%**start of header"),
          infocmd("setfilename", string_esc(NameS)),
	  infocmd("settitle", TitleR),
	  backend_comment("@paragraphindent 0"),
	  infocmd("setchapternewpage", raw(NewPage)),
	  backend_comment("@footnotestyle separate"),
	  %
	  InR,
	  %
	  if(tex, [
	    backend_comment("@smallbook"),
	    infocmd(PaperTypeS),
	    infocmd("tolerance", raw("10000")),
	    infocmd("hbadness", raw("10000"))
	  ]),
	  %
	  infoenv("macro", raw("hfill"), [
	    infoenv("tex", [
	      % note: this is plain tex
	      raw_fc, raw("@hfill"), raw_nleb
	    ])
	  ]),
%% Not necessary if defined in texinfo.tex
%% 	format(OS, "@macro dotlessi\n", []),
%% 	format(OS, "@tex\n", []),
%% 	format(OS, "@ptexi\n", []),
%% 	format(OS, "@end tex\n", []),
%% 	format(OS, "@end macro\n", []),
%% 	format(OS, "@macro dotlessj\n", []),
%% 	format(OS, "@tex\n", []),
%% 	format(OS, "@j\n", []),
%% 	format(OS, "@end tex\n", []),
%% 	format(OS, "@end macro\n", []),
	  backend_comment("%**end of header")
        ].

define_indices([], _DocSt, []).
define_indices([IdxName|Is], DocSt, [IR|IRs]) :-
	typeindex(IdxName, IndexId, IType, _ITitle, _IComment),
	list_concat(["def", IType, "index"], DefCmd),
	% TODO: How many of those commands may appear?
	%       This may rule out the possibility of defining a list of
	%       info commands here. That would be the case, a special case
	%       may be a workaround.
	atom_codes(IndexId, IndexIdS),
	IR = infocmd(DefCmd, raw(IndexIdS)),
	define_indices(Is, DocSt, IRs).

fmt_section(SecProps, SectLabel0, TitleR, BodyR, DocSt, R) :-
	get_nodename(SectLabel0, SectLabel1),
	( section_prop(with_parent, SecProps) ->
	    docst_currmod(DocSt, Name0),
	    atom_codes(Name0, NameS0),
	    texinfo_escape(NameS0, NameS),
	    % TODO: info documentation says that parenthesis are not allowed either!
	    format_to_string("~s (~s)", [SectLabel1, NameS], SectLabel2),
	    TitleR2 = [TitleR, string_esc(" ("), tt(string_esc(NameS0)), string_esc(")")]
	; SectLabel2 = SectLabel1,
	  TitleR2 = TitleR
	),
	( section_prop(linktop, SecProps) ->
	    % TODO: missing escape of SectLabel?
	    SectR = [infocmd("node", [raw(SectLabel2), raw(", , , Top")]),
	             backend_comment(" node-name,  next,  previous,  up"),
		     SR]
	; % TODO: missing escape of SectLabel?
	  SectR = [infocmd("node", [raw(SectLabel2), raw(", next,  previous,  up")]),
	           backend_comment("node-name, next,  previous,  up"),
		   SR]
	),
	fmt_structuring(SecProps, TitleR2, SR),
	R = [SectR, BodyR].

:- pred get_nodename(SectLabel, L) :: doclabel * string # "Obtain the node name".
get_nodename(global_label(SectLabel0), SectLabel) :- !,
	fix_nodename(SectLabel0, SectLabel).
get_nodename(local_label(SectLabel0), SectLabel) :- !,
	% TODO: sure?
	fix_nodename(SectLabel0, SectLabel).
get_nodename(localnum_label(SectLabel0), SectLabel) :- !,
	% TODO: sure?
	fix_nodename(SectLabel0, SectLabel).

% TODO: there are many other commands in info
fmt_structuring(SecProps, TitleR, R) :-
	( section_prop(unnumbered, SecProps) -> Cmd = "unnumbered"
	; section_prop(level(Level), SecProps) ->
	    ( Level = 1 -> Cmd = "chapter"
	    ; Level = 2 -> Cmd = "section"
	    ; Level = 3 -> Cmd = "subsection"
	    )
	; throw(missing_level_prop(SecProps))
	),
	R = infocmd(Cmd, TitleR).

:- doc(fix_nodename/2, "Issue an error message
   if the section name contains characters that info will not like
   and try to fix it within what is allowed by texinfo.").

fix_nodename(Section, NSection) :-
	fix_nodename_(Section, 0, Section, NSection).

fix_nodename_([],    _PrevChar, _Section, []).
fix_nodename_([H|T], _PrevChar, Section,  [NH|NT]) :-
	illegal_texinfo_section_name_char(H, NH),
	!,
%%% Since this is so specific to info, no warning any more 
%%% (simply fix by eliminating the char(s), and that is all).  
%% 	warning_message(
%% """~s"": info format does not support char ""~c"" in section names; replaced with ""~c""",
%% 	    [Section, H, NH]),
	fix_nodename_(T, H, Section, NT).
fix_nodename_([H|T], PrevChar, Section, T) :-
	illegal_texinfo_section_name_repeated_char(H),
	PrevChar == H,
	!,
%%% Since this is so specific to info, no warning any more 
%%% (simply fix by eliminating the char(s), and that is all).  
%% 	warning_message(
%% """~s"": info format does not support chars ""~c~c"" in section names; character ""~c"" deleted",
%% 	    [Section, H, H, H]),
	fix_nodename_(T, H, Section, T).
fix_nodename_([H|T], _PrevChar, Section, [H|NT]) :-
	fix_nodename_(T, H, Section, NT).

:- pred illegal_texinfo_section_name_char/2.

% illegal_texinfo_section_name_char(0'. ,0'  ). % Can actually be used?
illegal_texinfo_section_name_char(0',, 0' ).
illegal_texinfo_section_name_char(0':, 0'-).
illegal_texinfo_section_name_char(0'', 0' ).
illegal_texinfo_section_name_char(0'@, 0' ). % No commands recommended

:- regtype illegal_texinfo_section_name_repeated_char/1.

illegal_texinfo_section_name_repeated_char(0'-).

%% ---------------------------------------------------------------------------
:- pred texinfo_escape/2 : string * term => string * string

# "Escape @@, @{, and @} in strings. Also, converts tabs to 8 spaces:
   not ideal, but sufficient in many cases. There is an additional
   complication with &, which has to be treated differently in index
   entries (!).".
%% ---------------------------------------------------------------------------

texinfo_escape([],      []).
texinfo_escape([0'@|S], [0'@, 0'@|ES]) :- !,
	texinfo_escape(S, ES).
texinfo_escape([0'{|S], [0'@, 0'{|ES]) :- !,
	texinfo_escape(S, ES).
texinfo_escape([0'}|S], [0'@, 0'}|ES]) :- !,
	texinfo_escape(S, ES).
%% Escape not needed any more? -MH
%% texinfo_escape([0'&|S], [0'@, 0'&|ES]) :-
%% 	!,
%% 	texinfo_escape(S, ES).
texinfo_escape([0'\t|S], [0' , 0' , 0' , 0' , 0' , 0' , 0' , 0' |ES]) :- !,
	texinfo_escape(S, ES).
texinfo_escape([C|S], [C|ES]) :-
	!,
	texinfo_escape(S, ES).

% like texinfo_escape but do not expand tabs
% TODO: I am not sure about this --JF
texinfo_escape_verb([],      []).
texinfo_escape_verb([0'@|S], [0'@, 0'@|ES]) :- !,
	texinfo_escape_verb(S, ES).
texinfo_escape_verb([0'{|S], [0'@, 0'{|ES]) :- !,
	texinfo_escape_verb(S, ES).
texinfo_escape_verb([0'}|S], [0'@, 0'}|ES]) :- !,
	texinfo_escape_verb(S, ES).
texinfo_escape_verb([C|S], [C|ES]) :-
	!,
	texinfo_escape_verb(S, ES).

% %% Terrible kludge because of texinfo's very weird treatment of '&': has 
% %% to be escaped in, e.g., deffn, but not in index entries!
% unescape_ampersands_str([],           []).
% unescape_ampersands_str([0'@, 0'&|R], [0'&|NR]) :- !,
% 	unescape_ampersands_str(R, NR).
% unescape_ampersands_str([X|R], [X|NR]) :-
% 	unescape_ampersands_str(R, NR).

% ===========================================================================

:- multifile autodoc_finish_hook/1.
autodoc_finish_hook(texinfo) :- finish_texinfo.

% Obtain the .texi file for mainmod and its base name (useful to compose
% the name of some outputs or auxiliary files, e.g., logs).
texi_file_and_base(File, Base) :-
	main_absfile_in_format('texi', File),
	atom_concat(Base, '.texi', File).

% Group the .texic files, updating menus (local table of contents) and
% pointers, and saves a .texi file.
% (note: it uses emacs and emacs-library.el)
% TODO: This would not be hard to do in Ciao now --JF
% TODO: support for custom directories made this code really twisted

finish_texinfo :-
	Mod = ~get_mainmod,
	% @var{In} is the .texic file, @var{Out} the .texi file
	absfile_for_subtarget(Mod, texinfo, cr, In),
	texi_file_and_base(Out, FileBase),
	% Dummy file name (emacs-library.el does not like rel/full
	% paths for outfile)
	atom_concat(Mod, 'dummy', Dummy),
	% Obtain the directory where In lives
	get_name(In, InName),
	atom_concat(InDir, InName, In),
	% Path to Dummy
	atom_concat(InDir, Dummy, AbsDummy),
	% Invoke emacs-library.el (through emacs)
	setting_value(lpdoclib, LibDir),
 	working_directory(WD,WD),
 	cd(InDir),
	sh_exec([~emacs_for_ciao, ' -batch',
	     ' -l ', LibDir, 'emacs-library.el',
	     ' -f update-all-nodes-in-one-file',
	     ' -file ', InName,
	     ' -outfile ', Dummy],
	     [default, logbase(~atom_concat(FileBase, '_el')), no_throw]),
 	cd(WD),
	% Move AbsDummy to Out
	move_file(AbsDummy, Out),
	%
	generate_infoindex.

move_file(In, Out) :-
	sh_exec(['mv ', In, ' ', Out], []).

:- use_module(library(system), [working_directory/2, cd/1]).
:- use_module(library(make(make_rt)), [get_name/2]).
:- use_module(ciaodesrc(makedir('ConfigValues')), [emacs_for_ciao/1]).

% TODO: Move next to autodoc:fmt_infodir_entry2. Put call there?
% TODO: Is .infoindex extension really necessary? Can it be something else?
generate_infoindex :-
	Mod = ~get_mainmod,
	infodir_base(Mod, ModInfodir),
	absfile_for_subtarget(ModInfodir, texinfo, cr, In),
	main_absfile_in_format('infoindex', Out),
	copy_with_perms(In, Out).

copy_with_perms(In, Out) :-
	-copy_file(In, Out, [overwrite]),
	DataMode = ~setting_value_or_default(perms),
	-set_perms(Out, DataMode).

% updateinfodir <- [] # "Update info directory for all docs in docdir" :-
% 	working_directory(WD,WD),
% 	setting_value(docdir,DocDir),
% 	make_dir_if_no_exist(DocDir,~get_execmode),	
% 	cd(DocDir),
% 	setting_value(infodir_headfile, IDH),
% 	cat(IDH,dir),
% 	cat_append(~ls('*.infoindex'),dir),
% 	setting_value(infodir_tailfile, IDT),
% 	cat_append(IDT,dir),
% 	-set_perms(dir,~get_datamode),
% 	cd(WD).

% ---------------------------------------------------------------------------

:- export(infodir_base/2). % TODO: Temporally exported for lpdoc.pl
infodir_base(Mod, ModInfodir) :-
	atom_concat(Mod, 'dir', ModInfodir).

% ---------------------------------------------------------------------------

:- use_module(lpdocsrc(src(autodoc_filesystem)), [clean_tex_intermediate/0]).
:- use_module(lpdocsrc(src(autodoc_aux)), [sh_exec/2]).

:- multifile autodoc_gen_alternative_hook/2.
autodoc_gen_alternative_hook(texinfo, Alt) :-
	texinfo_gen_alternative(Alt).

% TODO: Fix run_* so that logs are written in separate directories
% TODO: Computations are repeated! (not a problem if only PDF is generated)
texinfo_gen_alternative(dvi) :- !,
	texi_file_and_base(TexiFile, FileBase),
	DVIFile = ~atom_concat(FileBase, '.dvi'),
	do_texi_to_dvi(TexiFile, DVIFile, FileBase),
	clean_tex_intermediate.
%
texinfo_gen_alternative(ps) :- !,
	texi_file_and_base(TexiFile, FileBase),
	DVIFile = ~atom_concat(FileBase, '.dvi'),
	PSFile = ~atom_concat(FileBase, '.ps'),
	do_texi_to_dvi(TexiFile, DVIFile, FileBase),
	do_dvi_to_ps(DVIFile, PSFile, FileBase),
	clean_tex_intermediate.
%
texinfo_gen_alternative(pdf) :- !,
	texi_file_and_base(TexiFile, FileBase),
	DVIFile = ~atom_concat(FileBase, '.dvi'),
	PSFile = ~atom_concat(FileBase, '.ps'),
	PDFFile = ~atom_concat(FileBase, '.pdf'),
	do_texi_to_dvi(TexiFile, DVIFile, FileBase),
	do_dvi_to_ps(DVIFile, PSFile, FileBase),
	do_ps_to_pdf(PSFile, PDFFile, FileBase),
	clean_tex_intermediate.
%
texinfo_gen_alternative(info) :- !,
	texi_file_and_base(TexiFile, FileBase),
	atom_concat(FileBase, '.info', InfoFile),
	atom_concat(FileBase, '.infoindex', InfoindexFile),
	do_texi_to_info(TexiFile, InfoFile, InfoindexFile, FileBase).
%
texinfo_gen_alternative(ascii) :- !,
	texi_file_and_base(TexiFile, FileBase),
	AsciiFile = ~atom_concat(FileBase, '.ascii'),
	do_texi_to_ascii(TexiFile, AsciiFile, FileBase).
%
texinfo_gen_alternative(rtf) :- !,
	texi_file_and_base(TexiFile, FileBase),
	RTFFile = ~atom_concat(FileBase, '.rtf'),
	do_texi_to_rtf(TexiFile, RTFFile, FileBase).

%% 'HLP' <= rtf :: FileBase :-
%%      do_hlp_from_rtf(FileBase).
%%
%% do_hlp_from_rtf(FileBase) :-
%% 	sh_exec([~rtftohlp,' ',FileBase], [default]),
%% 	-(set_perms(~atom_concat([FileBase,'.HLP']),DataMode)).

%% This depends on how smart your ~tex and ~texindex are...
do_texi_to_dvi(TexiFile, _DVIFile, FileBase) :-
	copy_texinfo_style_if_needed,
	TexCmd = [~tex, ' -file-line-error-style', ' ''\\nonstopmode\\input ', TexiFile, ''''],
	% TODO: allowing errors here, fix?
	sh_exec(TexCmd, [logbase(~atom_concat(FileBase, '_tex'))]),
	% TODO: replace .?? by the real suffixes: .li, .pd, .pr, .te, .de, .co, .gl, .au, etc.
	sh_exec([~texindex, ' ', FileBase, '.??'],
                [default, logbase(~atom_concat(FileBase, '_tex1'))]),
	% TODO: allowing errors here, fix?
	sh_exec(TexCmd, [logbase(~atom_concat(FileBase, '_tex2'))]).

copy_texinfo_style_if_needed :-
	( setting_value(libtexinfo, no) ->
	    note_message("Using external texinfo.tex style")
	; setting_value(lpdoclib, LibDir),
	  -copy_file(~atom_concat(LibDir, 'texinfo.tex'), 'texinfo.tex', [overwrite]),
	  note_message(note, "Using internal texinfo.tex style", [])
	).

do_dvi_to_ps(DVIFile, PSFile, FileBase) :-
	sh_exec([~dvips, ' ', DVIFile, ' -o ', PSFile],
	        [default, logbase(~atom_concat(FileBase, '_dvips'))]),
	% This, really to fix a bug in some versions of dvips:
	-(del_files_nofail(['head.tmp', 'body.tmp'])).

% (Using ps2pdf)
% Good for ps figures, but must make sure that no bitmap fonts are
% generated (at least -Ppdf in dvips MUST be set)
% TODO: Use pdftex instead?
do_ps_to_pdf(PSFile, PDFFile, FileBase) :-
	setting_value_or_default(papertype, PaperType),
	ghostscript_papertype(PaperType, GSPaperType),
	sh_exec(['GS_OPTIONS=-sPAPERSIZE=', GSPaperType, ' ', ~ps2pdf, ' ',
		PSFile, ' ', PDFFile],
                [default, logbase(~atom_concat(FileBase, '_ps2pdf'))]).

ghostscript_papertype(letterpaper, letter).
ghostscript_papertype(smallbook,   isob5). % This is an approximation
ghostscript_papertype(afourpaper,  a4).
ghostscript_papertype(afourlatex,  a4).
ghostscript_papertype(afourwide,   a4).
ghostscript_papertype(afourthesis, a4).

% TODO: move to autodoc_texinfo
%% Not in all distributions: --force (only in newer versions of texinfo), 
%% but needed: otherwise incomplete info file generated if there are any errors
%% As an alternative, set error limit very high... --error-limit 100000
do_texi_to_info(TexiFile, InfoFile, InfoindexFile, FileBase) :-
	% bold_message("Generating info file and index for ~w using ~w",
	%    [FileBase, ~makeinfo]),
	atom_concat(FileBase, '.info.tmp', TmpFile),
	% TODO: allowing errors here, fix
	sh_exec([~makeinfo,
		 ' --error-limit 100000 --force --no-split --verbose ',
		 '--fill-column=70 --output ', TmpFile, ' ',
		 TexiFile],
		[no_throw, logbase(~atom_concat(FileBase, '_info'))]),
	file_to_string(InfoindexFile, InfoIndex),
	% TODO: "Ciao System Manuals" should be configurable
	string_to_file(
                ~append("INFO-DIR-SECTION Ciao System Manuals\n"||
                        "START-INFO-DIR-ENTRY\n" || InfoIndex,
			"END-INFO-DIR-ENTRY\n"), InfoFile),
	copy_file(TmpFile, InfoFile, [append]),
	delete_file(TmpFile).

do_texi_to_ascii(TexiFile, AsciiFile, FileBase) :-
	% TODO: move to autodoc_texinfo
	sh_exec([~makeinfo,
		' --no-validate --error-limit 100000 --force ',
		'--no-split --verbose --no-headers --fill-column=70 ',
		'--output ', AsciiFile, ' ', TexiFile],
		[default, logbase(~atom_concat(FileBase, '_ascii'))]).

do_texi_to_rtf(TexiFile, RTFFile, FileBase) :-
	sh_exec([~makertf, ' --hpj ', FileBase, '.hpj -o ',
		RTFFile, ' ', TexiFile],
		[default, logbase(~atom_concat(FileBase, '_rtf'))]).

:- use_module(library(file_utils), [file_to_string/2, string_to_file/2]).
:- use_module(library(make(system_extra)), [(-)/1, copy_file/3, set_perms/2, del_files_nofail/1]).
:- use_module(library(system), [delete_file/1]).



