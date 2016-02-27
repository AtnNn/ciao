:- module(autodoc_xml, [], []).

:- use_package([assertions, regtypes, isomodes]).
:- use_package([contextual]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [list_concat/2, append/3]).
%:- use_module(library(format), [format_to_string/3]).
:- use_module(fastformat, [format_to_string/3]).
:- use_module(xmlwrite, [xml_format_stream/2]).
:- use_module(lpdocsrc(src(autodocformats))).
:- use_module(lpdocsrc(src(comments)), [version_descriptor/1]).
% :- use_module(lpdocsrc(src(postproc_xml))).
:- use_module(lpdocsrc(src(generate_xml))).
:- use_module(lpdocsrc(src(xml_store))).

%% ---------------------------------------------------------------------------
% TODO: This clones the general docstree_to_string for the XML backend
%       It should disappear eventually.

% (simple version)
% A reduced version of fmt_out that outputs a string
% TODO: This is required for @section{}, unless the command argument
%       is limited to something simpler than a doc_string
% TODO: I would like to get rid of this and rely only on fmt_out
doctree2str(X, DocSt, Ys) :-
	prepare_out(X, DocSt, X2),
	doctree2str_(X2, Ys, []).
%	doctree2str_(X, Ys, []).

doctree2str_(X, [X|Ys], Ys) :- var(X), !.
doctree2str_([], Xs, Xs) :- !.
doctree2str_([X|Xs], Ys, Zs) :- !,
	doctree2str_(X, Ys, Ys1),
	doctree2str_(Xs, Ys1, Zs).
doctree2str_(raw(Xs), Ys, Zs) :- !,
	append(Xs, Zs, Ys).
doctree2str_(raw_string(A), Ys, Zs) :- !,
	append(A, Zs, Ys).
doctree2str_(string_esc(A), Ys, Zs) :- !,
	append(A, Zs, Ys).
doctree2str_(string_verb(A), Ys, Zs) :- !,
	append(A, Zs, Ys).
doctree2str_(raw_nl, Ys, Zs) :- !,
	Ys = "\n"||Zs.
doctree2str_(X, [X|Xs], Xs) :-
	throw(unknown_dstr_concat(X)).

:- use_module(library(lists), [list_concat/2, append/3]).

prepare_out(R0, DocSt, R) :-
	rewrite_command(R0, DocSt, R1),
	dstr_fix_blanks(R1, R).

rewrite_command(A, _DocSt, A) :- ( A = raw(_) ; A = nop ; A = raw_string(_) ), !.
rewrite_command(A, DocSt, B) :- ( A = [] ; A = [_|_] ), !, rewrite_commands(A, DocSt, B).
rewrite_command(Command, DocSt, NewCommand) :-
	functor(Command, Cmd, A),
	functor(BT, Cmd, A),
	( cmd_type(BT) ->
	    Command =.. [_|Xs],
	    BT =.. [_|Ts],
	    rewrite_cmd_args(Ts, Xs, DocSt, Ys),
	    B1 =.. [Cmd|Ys],
	    xml_rewrite_command(B1, DocSt, NewCommand)
	; icmd_type(BT) ->
	    Command =.. [_|Xs],
	    BT =.. [_|Ts],
	    rewrite_cmd_args(Ts, Xs, DocSt, Ys),
	    NewCommand =.. [Cmd|Ys]
	; throw(wrong_input(rewrite_command(Command)))
	).

rewrite_cmd_args([], [], _, []).
rewrite_cmd_args([T|Ts], [X|Xs], DocSt, [Y|Ys]) :-
	( T = d -> rewrite_command(X, DocSt, Y)
	; Y = X
	),
	rewrite_cmd_args(Ts, Xs, DocSt, Ys).

rewrite_commands([], _DocSt, []).
rewrite_commands([X|Xs], DocSt, [Y|Ys]) :-
	rewrite_command(X, DocSt, Y),
	rewrite_commands(Xs, DocSt, Ys).

xml_rewrite_command(Command, _DocSt, Body) :-
	functor(Command, Cmd, A),
	functor(CommandT, Cmd, A),
	cmd_type(CommandT),
	CommandT =.. [_|Ts],
	Command =.. [F|Args0],
	repack_args(Ts, Args0, Args),
	atom_codes(F, FS),
	( Args = [] ->
	    Body = [raw("@"), raw(FS), raw(" ")]
	; Args = [Arg] ->
	    Body = [raw("@"), raw(FS), raw("{"), Arg, raw("}")]
	; Args = [Arg1, Arg2] ->
	    Body = [raw("@"), raw(FS), raw("{"), Arg1, raw("}{"), Arg2, raw("}")]
	).

repack_args([], [], []).
repack_args([T|Ts], [X|Xs], [Y|Ys]) :-
	( T = d -> Y = X
	; % T = s ->
	    Y = raw(X)
	),
	repack_args(Ts, Xs, Ys).

% ======================================================================

:- multifile lpdoc_format_module_hook/24.

lpdoc_format_module_hook(
     DocSt,FileType,ModuleType,_DirName,Name,NDName,
     %
     State,
     %
     _StartPage,_PaperType,
     %
     Version,GVers,
     %
     TitleR,AuthorRs,SubtitleRs,CopyrightR,SummaryR,
     %
     CommentR,
     %
     _InterfaceR,
     %
     AppendixR, AckR, Changes, BugRs,
     %
     Components,
     %
     ModR) :-
	docstate_format(DocSt, xml),
	!,
	ModR = [],
	( xml_format_front_matter(
            DocSt, ModuleType, FileType, Name, NDName,
	    Version, GVers, TitleR, AuthorRs, SubtitleRs,
	    CopyrightR, SummaryR),
	  doctree2str(CommentR, DocSt, Comment2),
	  module_elem('front-matter', 'comment', [], [Comment2]),
	  xml_format_other_info(DocSt, State, AppendixR, AckR, Changes, BugRs) ->
	    true
	; true
	),
	( FileType = main(_) ->
	    xml_format_includes_and_end_matter(DocSt, Name, Components)
	; true
	).

% ......................................................................

:- pred xml_format_front_matter(DocSt, ModuleType, MainOrComp, Name,
	    NDName, Version, GVers, TitleR, AuthorRs, SubtitleRs,
	    CopyrightR, SummaryR) : docstate * atm * term * atm * atm *
	    version_descriptor * version_descriptor * doctree *
	    list(doctree) * list(doctree) * doctree * doctree.

xml_format_front_matter(DocSt, ModuleType, MainOrComp, Name, NDName, Version,
	    GVers, TitleR, AuthorRs, SubtitleRs, CopyrightR, SummaryR) :-
        %
	data_facts:retractall_fact(xml_content(_, _)),
        %
	atom_codes(NDName, NDNameS),
	( is_empty_doctree(TitleR) -> Title2 = NDNameS
	; doctree2str(TitleR, DocSt, Title2)
	),
	( MainOrComp=component -> Comp2=true
	; Comp2= false
	),
        %
	module_elem('front-matter', 'title',
	    ['module-type'=ModuleType,
		'module-name'=Name,
		'is-component'=Comp2,
		'module-nd-name'=NDName], [Title2]),
	dstr_list_to_string_list(SubtitleRs, DocSt, Subtitle2),
	module_elem_each('front-matter', 'subtitle', Subtitle2),
	dstr_list_to_string_list(AuthorRs, DocSt, Authors2),
	module_elem_each('front-matter', 'author', Authors2),
	doctree2str(CopyrightR, DocSt, Copyright2),
	module_elem_each('front-matter', 'copyright', [Copyright2]),
	doctree2str(SummaryR, DocSt, Summary2),
	module_elem_each('front-matter', 'summary', [Summary2]),
	module_version('front-matter', 'global-version', GVers),
	module_version('front-matter', 'last-change-version', Version).

dstr_list_to_string_list([], _, []).
dstr_list_to_string_list([A|As], DocSt, [B|Bs]) :-
	doctree2str(A, DocSt, B),
	dstr_list_to_string_list(As, DocSt, Bs).

module_version(Section, Item, Ver) :-
	( Ver= version(Major*Minor+Patch, Y/M/D, H:M*S+Z) -> true
	; Ver= version(Major*Minor+Patch, Y/M/D), H=0, M=0, S=0, Z=unknown
	),
	format_to_string("~w-~w-~w", [Y, M, D], DateString),
	module_elem(Section, Item,
	    [major=Major, minor=Minor, patch=Patch, year=Y,
		month=M, day=D, hour=H, minutes=M, seconds=S, zone=Z],
	    [DateString]).

% ======================================================================

:- multifile lpdoc_format_module_usage_hook/13.

lpdoc_format_module_usage_hook(DocSt, Name, Type, Exports,
	    Mults, UMods, IUMods, SysMods, EngMods, Ops,
	    NDecls, NModes, R) :-
        docstate_format(DocSt, xml),
	!,
	R = [],
	xml_format_module_usage(DocSt, Name, Type, Exports, Mults, UMods,
	    IUMods, SysMods, EngMods, Ops, NDecls, NModes).

% ......................................................................

:- pred xml_format_module_usage(DocSt, Name, Type, CExports, Mults, UMods,
	    IUMods, SysMods, EngMods, Ops, NDecls, NModes) : atm * atm *
	docstate * list(predname) * list(atm) * list(atm) * list(atm) * list(
	    atm) * list(atm) * list * list(atm) * list(atm).

xml_format_module_usage(DocSt, Name, Type, CExports, Mults, UMods, IUMods,
	    SysMods, EngMods, Ops, NDecls, NModes) :-
        %
%	format( ">>> MODULE USAGE ENTRY!!!~n", []),
        %
	( Type= comment(CommentR) -> UType = comment, doctree2str(CommentR, DocSt, S)
	; UType= Type, atom_codes(Name, S)
	),
        %
	module_elem('module-usage', 'usage-info',
	    [type=UType], [S]),
        %
	handle_exports(["PREDICATE" /'predicate',
		"FUNCTION" /'function', "PROPERTY" /'property',
		"REGTYPE" /'regtype', "ENTRY POINT" /'entry-point'],
	    CExports),
        %
	handle_export_cases(Mults, 'multi-file'),
	format_ops(Ops),
        %
	handle_declarations('mode', NModes, global),
        %
	handle_declarations('new', NDecls, global),
        %
	handle_luc(UMods,   'application'),
	handle_luc(IUMods,  'user'),
	handle_luc(SysMods, 'system'),
	handle_luc(EngMods, 'engine'). %,
%	format( ">>> MODULE USAGE SUCCESS!!!~n", []).

handle_exports([],                    _).
handle_exports([TypeStr/Attrib|Rest], CExports) :-
	filter_exports(TypeStr, CExports, FAList),
	handle_export_cases(FAList, Attrib),
	handle_exports(Rest, CExports).

filter_exports(_,       [],                       []).
filter_exports(TypeStr, [export(F/A, Type)|Rest], FAList) :-
	( typetext(Type, TypeStr, _, _) -> FAList= [F/A|FARest],
	    filter_exports(TypeStr, Rest, FARest)
	; filter_exports(TypeStr, Rest, FAList)
	).

handle_export_cases([],         _).
handle_export_cases([F/A|Rest], Attrib) :-
	format_to_string("~w/~w", [F, A], S),
	atom_codes(Sig, S),
	module_elem('module-usage', 'export',
	    [type=Attrib, predicate=F, arity=A, signature= Sig], []),
	handle_export_cases(Rest, Attrib).

handle_declarations(_,      [],          _).
handle_declarations(Attrib, [Decl|Rest], DType) :-
	( Decl = F/A -> Descrip= [predicate=F, arity= A]
	; format_to_string("~w", [Decl], S),
	    atom_codes(SA, S),
	    Descrip= [target=SA]
	),
	module_elem('module-usage', 'declaration',
	    [type=Attrib|Descrip], []),
	handle_declarations(Attrib, Rest, DType).

handle_luc([],         _).
handle_luc([Mod|Rest], Attrib) :-
	module_elem('module-usage', 'use-module',
	    [type=Attrib, name=Mod], []),
	handle_luc(Rest, Attrib).

format_ops([]).
format_ops([op(Prec, Type, Functor)|Ops]) :-
	module_elem('module-usage', 'operator',
	    [precedence=Prec, type=Type, functor=Functor], []),
	format_ops(Ops).

% ======================================================================

:- pred xml_format_includes_and_end_matter(DocSt, Name, Components) :
	docstate * atm * list(atm).

xml_format_includes_and_end_matter(DocSt, Name, Components) :-
	format_to_string("~w", [Name], NameS),
	module_elem('end-matter', 'module-name', [], [NameS]),
	module_elem_each('end-matter', 'component',   Components),
	docstate_opts(DocSt, Opts),
	% TODO: do not store the options there!
	module_elem_each('end-matter', 'used-option', Opts).

% ======================================================================

:- multifile lpdoc_format_predicate_begin_hook/4.

lpdoc_format_predicate_begin_hook(DocSt, Type, Pred, R) :-
	docstate_format(DocSt, xml),
	!,
	R = [],
	xml_format_predicate_begin(DocSt, Type, Pred).

% ......................................................................

:- pred xml_format_predicate_begin(DocSt, Type, Pred) :
	docstate * term * predname.

xml_format_predicate_begin(DocSt, Type, F/A) :-
%	format( "---- ~w ---(1)~n", [F/A]),
	retractall_fact(xml_pred(_, _, _)),
%	format( "---- ~w ---(1a)~n", [F/A]),
	retractall_fact(xml_pred_comment(_)),
%	format( "---- ~w ---(1b)~n", [F/A]),
	retractall_fact(xml_pred_props(_)),
%	format( "---- ~w ---(2)~n", [F/A]),
	retractall_fact(xml_pred_prop_acc(_)),
	docstate_opts(DocSt, Opts),
	assertz_fact(xml_pred(Type, F/A, Opts)).

% ======================================================================

:- multifile lpdoc_format_predicate_comment_hook/3.

lpdoc_format_predicate_comment_hook(DocSt, Comment, R) :-
	docstate_format(DocSt, xml),
	!,
	R = [],
	xml_format_pred_comment(DocSt, Comment).

xml_format_pred_comment(DocSt, Comment0) :-
	doctree2str(Comment0, DocSt, Comment),
	assertz_fact(xml_pred_comment(env('comment', [], [Comment]))).

% ======================================================================

:- multifile lpdoc_format_native_declaration_hook/3.

lpdoc_format_native_declaration_hook(DocSt, Decl, R) :-
	docstate_format(DocSt, xml),
	!,
	R = [],
	xml_format_native_declaration(Decl).

% ......................................................................

:- pred xml_format_native_declaration(Decl) :: term.

xml_format_native_declaration(Decl) :-
	format_to_string("~w", [Decl], DeclS),
	assertz_fact(xml_pred_props(env('declaration',
		    [], [DeclS]))).

% ======================================================================

:- multifile lpdoc_format_head_descriptor_hook/5.

lpdoc_format_head_descriptor_hook(DocSt, HD, Type, Standard, HeadR) :-
	docstate_format(DocSt, xml),
	!,
	HeadR = [],
	xml_format_head_descriptor(HD, Type, Standard).

% ......................................................................

xml_format_head_descriptor(HD, _, Standard) :-
	format_to_string("~w", [HD], HeadStr),
	E=env(head, [standard=Standard], [HeadStr]),
%	display( head( E)),
	asserta_fact(xml_pred_head(E)).

% ======================================================================

:- multifile lpdoc_format_properties_begin_hook/2.

lpdoc_format_properties_begin_hook(DocSt, BeginR) :-
	docstate_format(DocSt, xml),
	!,
	BeginR = [],
	xml_format_properties_begin.

% ......................................................................

:- pred xml_format_properties_begin.

xml_format_properties_begin:-
	retractall_fact(xml_pred_descr(_)),
	retractall_fact(xml_prop_group(_)).

% ======================================================================

:- multifile lpdoc_format_site_begin_hook/4.

lpdoc_format_site_begin_hook(DocSt, Text, _Bullet, BeginR) :-
	docstate_format(DocSt, xml),
	!,
	BeginR = [],
	xml_format_site_begin(Text).

% ----------------------------------------------------------------------

xml_format_site_begin(Text) :-
	retractall_fact(xml_site_heading(_)),
	atom_codes(TextA, Text),
	asserta_fact(xml_site_heading(TextA)),
	retractall_fact(xml_pred_prop_acc(_)).

% ======================================================================

:- multifile lpdoc_format_site_end_hook/2.

lpdoc_format_site_end_hook(DocSt, EndR) :-
	docstate_format(DocSt, xml),
	!,
	EndR = [],
	xml_format_site_end.

% ----------------------------------------------------------------------

xml_format_site_end:-
	findall(Prop, xml_pred_prop_acc(Prop), Props),
	xml_site_heading(TextA),
	assertz_fact(xml_prop_group(env('prop-group', [text=TextA], Props))).

% ======================================================================

:- multifile lpdoc_format_property_hook/6.

lpdoc_format_property_hook(DocSt, Prop, PM, DocString, VarDict, PropR) :-
	docstate_format(DocSt, xml),
	!,
	PropR = [],
	xml_format_property(DocSt, Prop, PM, DocString, VarDict).

xml_format_property(DocSt, Prop, PM, DocString, VarDict) :-
	functor(Prop, PF, PA),
	format_to_string("~w", [Prop], PropS),
%	list_vars( VarNames, VarList),
	( DocString==undefined -> Doc2 = []
	; fill_vardict(VarDict),
	  doctree2str(DocString, DocSt, Doc2)
	),
% 	(  xml_site_heading( SiteHeading)
% 	-> true
% 	;  SiteHeading= env( noheading, [], [])
% 	),
%	[ SiteHeading, env( form, [], [PropS]), env( 'description', [], [Doc2])]))).
	assertz_fact(xml_pred_prop_acc(env(property, [module=PM,
			functor=PF, arity=PA],
		    [env(form, [], [PropS]), env('description', [], [Doc2])]))).

fill_vardict([]).
fill_vardict([X=V|Ds]) :-
	format_to_string("~w", [V], ES),
	X = var([string_esc(ES)]),
	fill_vardict(Ds).

:- use_module(library(format), [format/3]).

/*
highlight_vars([],    []).
highlight_vars([V|L], [VHL|R]) :-
	format_to_string("@var{~w}", [V], VHLS),
	atom_codes(VHL, VHLS),
	highlight_vars(L, R).

list_vars([],       []).
list_vars([V|Rest], [env(variable, [name=V], [])|RestList]) :-
	list_vars(Rest, RestList).
*/
% ======================================================================

:- multifile lpdoc_format_description_hook/3.

lpdoc_format_description_hook(DocSt, Desc, DescR) :-
	docstate_format(DocSt, xml),
	!,
	DescR = [],
	xml_format_description(DocSt, Desc).

xml_format_description(DocSt, Desc) :-
	doctree2str(Desc, DocSt, DescR),
	assertz_fact(xml_pred_descr(env(description, [], [DescR]))).

% ======================================================================

:- multifile lpdoc_format_properties_end_hook/2.

lpdoc_format_properties_end_hook(DocSt, EndR) :-
	docstate_format(DocSt, xml),
	!,
	EndR = [],
	xml_format_properties_end.

% ......................................................................

:- pred xml_format_properties_end.

xml_format_properties_end:-
	findall(X, xml_prop_group(X), PropsAcc),
	( xml_pred_descr(Desc)
	-> List1= [Desc|PropsAcc]
	; List1= PropsAcc
	),
	( xml_pred_head(Head)
	->  List2= [Head|List1]
	;    List2= List1
	),
	assertz_fact(xml_pred_props(env(properties, [], List2))).

% ======================================================================

:- multifile lpdoc_format_predicate_end_hook/2.

lpdoc_format_predicate_end_hook(DocSt, R) :-
	docstate_format(DocSt, xml),
	!,
	R = [],
	xml_format_predicate_end.

% ......................................................................

:- pred xml_format_predicate_end.

xml_format_predicate_end:-
	xml_pred(Type, F/A, _),
	findall(X, xml_pred_comment(X), Comments),
	findall(X, xml_pred_props(X),   Properties),
	append(Comments, Properties, Content),
	module_elem('predicates', 'predicate',
	    [type=Type, functor=F, arity=A], Content).

% ======================================================================

xml_format_other_info(DocSt, State, AppendixR, AckR, Changes, BugRs) :-
	extstate_filetype(State, FileType),
	extstate_name(State, Name),
	extstate_ndname(State, NDName),
	format_to_string("~w", FileType, FileTypeS),
	format_to_string("~w", Name,     NameS),
	format_to_string("~w", NDName,   NDNameS),
        %
	module_elem('other-info', 'file-type',        [], [FileTypeS]),
	module_elem('other-info', 'module-name',      [], [NameS]),
	module_elem('other-info', 'module-nd-name',   [], [NDNameS]),
	doctree2str(AppendixR, DocSt, Appendix2),
	module_elem('other-info', 'appendix',         [], [Appendix2]),
	doctree2str(AckR, DocSt, Ack2),
	module_elem('other-info', 'acknowledgements', [], [Ack2]),
	% TODO: missing translation, wrong
	store_changes(Changes),
	module_elem_each('other-info', 'bug', BugRs).

store_changes([]).
store_changes([change(Version, Change)|Rest]) :-
	( Version= version(Major*Minor+Patch, Y/M/D) -> H=0, M=0, S=0, Z='?'
	; Version= version(Major*Minor+Patch, Y/M/D, H:M*S+Z) -> true
	; Major='?', Minor='?', Patch='?', Y='?', M='?', D='?', H=0, M=0,
	    S=0, Z='?'
	),
	module_elem('other-info', 'change',
	    [major=Major, minor=Minor, patch=Patch, year=Y, month=M, day=D,
		hour=H, minutes=M, seconds=S, zone=Z], [Change]),
	store_changes(Rest).

% ----------------------------------------------------------------------


% ======================================================================

:- multifile lpdoc_format_before_close_hook/2.

lpdoc_format_before_close_hook(DocSt, OS) :-
	docstate_format(DocSt, xml),
	!,
	xml_format_before_close(DocSt, OS).

% ......................................................................

xml_format_before_close(_DocSt, OS) :-
%	module_collect_all( M),
%	format( "After module collect all: M=~q~n", [M]),
%	XML= [M],
	generate_xml(XML), !,
%	format("After generate_xml~n", []),
%	display( XML),
	xml_format_stream(OS, XML), !,

%	format("Flushing XML to ~q...", [OS]),

%	format( OS, "~N<hr/>~N<pre>~N", []),
%	module_collect_all( M),
%	xml_format_stream( OS, [M]),
%	format( OS, "~N</pre>~N", []),

%	format( OS, "<!-- END -->~n", []),
%	format("After xml_format~n", []).
	true.

% ======================================================================

dstr_fix_blanks(A, A) :- A = raw(_), !.
dstr_fix_blanks(A, B) :- ( A = [] ; A = [_|_] ), !, dstr_fix_blanks_list(A, B). 
dstr_fix_blanks(raw_string(String), raw(String)) :- !.
dstr_fix_blanks(Command, B) :-
	functor(Command, Cmd, A),
	functor(BT, Cmd, A),
	icmd_type(BT),
	!,
	Command =.. [_|Xs],
	BT =.. [_|Ts],
	fix_blanks_args(Ts, Xs, Ys),
	B =.. [Cmd|Ys].
dstr_fix_blanks(A, _) :-
	throw(wrong_input(dstr_fix_blanks(A))).

fix_blanks_args([], [], []).
fix_blanks_args([T|Ts], [X|Xs], [Y|Ys]) :-
	( T = d -> dstr_fix_blanks(X, Y)
	; Y = X
	),
	fix_blanks_args(Ts, Xs, Ys).

dstr_fix_blanks_list([], []) :- !.
dstr_fix_blanks_list([R0|Rs], [R|Tail]) :-
	dstr_fix_blanks(R0, R),
	dstr_fix_blanks_list(Rs, Tail).


