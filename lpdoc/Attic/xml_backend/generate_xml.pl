:- module(generate_xml, [], [contextual]).

:- use_module(lpdocsrc(src(postproc_xml))).
:- use_module(library(format), [format/3]).
%:- use_module(library(format), [format_to_string/3]).
:- use_module(fastformat, [format_to_string/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(lpdocsrc(src(xml_store))).
:- use_module(library(file_utils), [file_to_string/2]).

% ======================================================================

member_nb(X, Xs) :- member(X, Xs), !.

% ======================================================================

:- export(generate_xml/1).
generate_xml(Html) :-
	generate_xml1(Html, []).

% TODO: add doctype?
%   <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
generate_xml1 / html :-
	default_env / -penv,
	generate_head / (penv, Head-[]),
	generate_body / (penv, Body-[]),
	[env(html, [], [
		    env(head, [], Head),
		    env(body, [], Body)
		])] / html.

% ======================================================================

generate_head / (penv, head) :-
	get_title_string(TitleString),
	absolute_file_name(lpdocsrc('src/doc_styles.css'), CSSFile),
	file_to_string(CSSFile, StyleDefs),
	[env(title, [], [TitleString])] / head,
	[
         env(meta,
		['http-equiv'='Content-Type',
		    content='text/html; charset=utf-8'
		], []),
%
%	    env( link,
%	    [ rel=stylesheet, type='text/css', href='doc_styles.css'
%	    ], [])
	    env(style,
		[type='text/css'],
		[StyleDefs])
	] / head.

% ......................................................................

get_title_string(TitleString) / (*) :-
	( xml_content('front-matter', env(title, Attribs, [TitleDS])) ->
	    ( TitleDS\= [] ->
	        flatten(TitleDS, TitleString)
	    ; member_nb('module-nd-name'=NDName, Attribs) ->
	        format_to_string("~w - Ciao Documentation",
		  [NDName], TitleString)
	    ; member_nb('module-name'=ModName, Attribs) ->
	        format_to_string("~w - Ciao Documentation",
		  [ModName], TitleString)
	    )
	), !.
get_title_string("Ciao Documentation for source file").

% ======================================================================

generate_body / (penv, body) :-
	gen_front_matter / (penv, body),
	( xml_content('front-matter', env(comment, _, [DS])),
	  DS \== [] ->
	    add_postproc_content / (+DS, penv, body)
	; true
	),
	gen_module_usage / (penv, body),
	gen_document_exports / (penv, body),
	gen_end_matter / (penv, body),
	gen_footnotes / (penv, body),
	current_file(FileS) / +penv,
	atom_codes(FileA, FileS),
	gen_indices(FileA) / (penv, body).

% ----------------------------------------------------------------------

gen_front_matter / (penv, body) :-
	% Put title
	( xml_content('front-matter', env(title, Attribs, [TitleDS])) ->
	    flatten(TitleDS, Title),
	    [env(h1, [], [Title])] / body
	; Attribs= [] % TODO: used?
	),
	% Put subtitle
	( xml_content('front-matter', env(subtitle, _, [SubtitleDS])) ->
	    flatten(SubtitleDS, Subtitle),
	    [env(p, [class=doc_subtitle], [Subtitle])] / body
	; true
	),
	% Create front table with summary information
	gen_front_table / (penv, body).

% ----------------------------------------------------------------------

gen_front_table / (penv, body) :-
	[env(h2,    [],                      ["General Information"])] / body,
	[env(table, [class=doc_front_table], Rows)] / body,
	gen_front_table_rows / (penv, Rows-[]).

% ----------------------------------------------------------------------

add_rows(Label, ContentDSL) / (penv, table) :-
	add_rows1(ContentDSL) / (penv, ContentL-[]),
	simplify_content(ContentL, ContentC),
	( Label=Class/L
	-> TheLabel= L, Attribs=[class=Class]
	; TheLabel= Label, Attribs=[]
	),
	( ContentC=[]
	-> true
	; [env(tr, Attribs,
		    [
			env(th, [valign=top], [TheLabel]),
			env(td, [],           ContentC)
		    ])
	    ] / table
	).

add_rows1([]) / (penv, content) :- true.
add_rows1([DS|L]) / (penv, content) :-
	add_postproc_content / (+DS, penv, content),
	add_rows1(L) / (penv, content).

% ----------------------------------------------------------------------

gen_front_table_rows / (penv, table) :-
% 	[ env( tr, [],
% 	  [ env( th, [colspan=2, class=doc_table_header], 
% 	    ["General Information"])
% 	  ])
% 	] / table,
	gather_module_name(ModuleNameDSL),
	add_rows("Module Name:", ModuleNameDSL) / (penv, table),
	gather_module_type(ModuleTypeDSL),
	add_rows("Module Type:", ModuleTypeDSL) / (penv, table),
	gather_is_component(IsComponentDSL),
	add_rows("Component?", IsComponentDSL) / (penv, table),
	gather_authors(AuthorsDSL),
	add_rows("Author(s):", AuthorsDSL) / (penv, table),
	gather_copyright(CopyrightDSL),
	add_rows("Copyright:", CopyrightDSL) / (penv, table),
%	gather_global_version( GVersDSL),
%	add_rows( "Global Version:", GVersDSL) / (penv, table),
%	gather_last_change_version( LCVersDSL),
%	add_rows( "Last Change Version:", LCVersDSL) / (penv, table),
	gather_summary(SummaryDSL),
	add_rows("Summary:", SummaryDSL) / (penv, table).

% ......................................................................

gather_authors(AuthorsDSL) :-
	findall(AuthorDS,
	    xml_content('front-matter', env(author, _, [AuthorDS])),
	    AuthorsDSL).

% ......................................................................

gather_copyright(CopyrightDSL) :-
	( xml_content('front-matter', env(copyright, _, [CopyrightDS]))
	-> CopyrightDSL= [CopyrightDS]
	; CopyrightDSL= []
	).

% ......................................................................

% gather_global_version([]).

% ......................................................................

% gather_last_change_version([]).

% ......................................................................-

gather_module_type(ModuleTypeDSL) :-
	( xml_content('front-matter', env(title, Attribs, _)),
	    member_nb('module-type'=ModuleTypeA, Attribs)
	-> format_to_string("~q", [ModuleTypeA], ModuleTypeDS),
	    ModuleTypeDSL= [ModuleTypeDS]
	; ModuleTypeDSL= []
	).

% ......................................................................-

gather_module_name(ModuleNameDSL) :-
	( xml_content('front-matter', env(title, Attribs, _)),
	    member_nb('module-nd-name'=ModuleNameA, Attribs)
	-> format_to_string("@bf{@tt{~w}}", [ModuleNameA], ModuleNameDS),
	    ModuleNameDSL= [ModuleNameDS]
	; ModuleNameDSL= []
	).

% ......................................................................

gather_is_component(IsComponentDSL) :-
	( xml_content('front-matter', env(title, Attribs, _)),
	    member_nb('is-component'=Flag, Attribs),
	    member_nb(Flag/Answer, [true/ "Yes", false/ "No"])
	-> IsComponentDSL= [Answer]
	; IsComponentDSL= []
	).

% ......................................................................

gather_summary(SummaryDSL) :-
	( xml_content('front-matter', env(summary, _, [SummaryDS]))
	-> SummaryDSL= [SummaryDS]
	; SummaryDSL= []
	).

% ----------------------------------------------------------------------

gen_end_matter / (penv, body) :-
	% Put one component
	gather_components(ComponentDSL),
        ( ComponentDSL = [] ->
	    true
	; [env(h2, [], ["List of Components"])] / body,
	  [env(ul, [], Rows)] / body,
	  add_components(ComponentDSL) / (Rows-[])
	).

add_components([]) / body :- true.
add_components([C|Cs]) / body :-
	add_component(C) / body,
	add_components(Cs) / body.

:- use_module(library(make(make_rt)), [map_target_base/4]).

add_component(CompDS) / body :-
	( atom_codes(Atm1, CompDS),
	  map_target_base(Atm1, pl, xml, Base), % TODO: hmmm...
	  atom_concat(Base, '.doc.html', Atm) ->
	    CompName = CompDS % TODO: Wrong
	; Atm = '',
	  CompName = "Failed link to "||CompDS,
	  format(user_error, "Failed component link: ~s~n", [CompDS])
	),
	flatten(CompName, CompName2),
	[env(li, [], [env(a, [href=Atm], [CompName2])])] / body.

gather_components(ComponentDSL) :-
	findall(ComponentDS,
	    xml_content('end-matter', env(component, _, [ComponentDS])),
	    ComponentDSL).

% ======================================================================

gen_module_usage / (penv, body) :-
	gen_module_usage_rows / (penv, UsageRows-[]),
	( UsageRows = [] ->
	    true
	; [env(h2, [], ["Usage and Interface"])] / body,
	  [env(table, [class=doc_usage_table], UsageRows)] / body
	).

% ......................................................................

% TODO: Many duplicated codea in autodoc_texinfo! 
gen_module_usage_rows / (penv, table) :-
% 	[ env( tr, [],
% 	  [ env( th, [colspan=2, class=doc_table_header], 
% 	    ["Module Usage And Interface"])
% 	  ])
% 	] / table,
%
	findall(
	    UsageDS,
	    ( xml_content('module-usage', env('usage-info', A, [S])),
		member_nb(type=UType, A),
		( UType = comment ->
		    UsageDS = S
		; UType = use_module ->
		    % TODO: library prefix hardwired!
		    format_to_string("@tt{:- ~q(library(~s)).}", [UType, S], UsageDS)
		; format_to_string("@tt{:- ~q(~s).}", [UType, S], UsageDS)
		)
	    ),
	    UsageDSL
	),
	add_rows("Library Usage:", UsageDSL) / (penv, table),
%
	gen_export_list(predicate, "predicates") / (penv, table),
	gen_export_list(function, "functions") / (penv, table),
	gen_export_list(property, "properties") / (penv, table),
	gen_export_list(regtype, "regtypes") / (penv, table),
	gen_export_list('entry-point', "entry points") / (penv, table),
	gen_export_list('multi-file', "multifiles") / (penv, table),
	gen_operators / (penv, table),
%
	findall(
	    UAppDS,
	    ( xml_content('module-usage', env('use-module', A, _)),
		member_nb(type=application, A),
		member_nb(name=UAppA,       A),
		format_to_string("~q", [UAppA], UAppDS)
	    ),
	    UAppDSL
	),
	( UAppDSL\=[]
	-> to_csl(UAppDSL) / (UAppCSL-[]),
	    add_rows("Uses application modules:", [UAppCSL]) / (penv, table)
	; true
	),
%
	findall(
	    UModDS,
	    ( xml_content('module-usage', env('use-module', A, _)),
		member_nb(type=user,  A),
		member_nb(name=UModA, A),
		format_to_string("~q", [UModA], UModDS)
	    ),
	    UModDSL
	),
	( UModDSL\=[]
	-> to_csl(UModDSL) / (UModCSL-[]),
	    add_rows("Uses user modules:", [UModCSL]) / (penv, table)
	; true
	),
%
	findall(
	    USysDS,
	    ( xml_content('module-usage', env('use-module', A, _)),
		member_nb(type=system, A),
		member_nb(name=USysA,  A),
		format_to_string("~q", [USysA], USysDS)
	    ),
	    USysDSL
	),
	( USysDSL\=[]
	-> to_csl(USysDSL) / (USysCSL-[]),
	    add_rows("Uses system modules:", [USysCSL]) / (penv, table)
	; true
	),
%
	findall(
	    UEngDS,
	    ( xml_content('module-usage', env('use-module', A, _)),
		member_nb(type=engine, A),
		member_nb(name=UEngA,  A),
		format_to_string("~q", [UEngA], UEngDS)
	    ),
	    UEngDSL
	),
	( UEngDSL\=[]
	-> to_csl(UEngDSL) / (UEngCSL-[]),
	    add_rows("Uses engine modules:", [UEngCSL]) / (penv, table)
	; true
	).

% ......................................................................

gen_export_list(Type, Plural) / (penv, table) :-
	findall(
	    PredDS,
	    (
		xml_content('module-usage', env(export, W, _)),
		member_nb(type=Type,   W),
		member_nb(predicate=F, W),
		member_nb(arity=A,     W),
		format_to_string("@pred{~q}", [F/A], PredDS)
	    ),
	    PredDSL
	),
	( PredDSL=[]
	-> true
	; to_csl(PredDSL) / (CSL-[]),
	    format_to_string("Exports ~s:", [Plural], Label),
	    add_rows(Label, [CSL]) / (penv, table)
	).

% ......................................................................

to_csl([]) / dl :- true.
to_csl([X]) / dl :-
	prefix(X) / dl.
to_csl([X, Y|L]) / dl:-
	prefix(X) / dl,
	", " / dl,
	to_csl([Y|L]) / dl.

% ......................................................................
/*
gen_decl_list(Type) / (penv, table) :-
	findall(
	    DeclDS,
	    (
		xml_content('module-usage', env(declaration, W, _)),
		member_nb(type=Type, W),
		( member_nb(target=Target, W)
		; member_nb(predicate=F, W),
		    member_nb(arity=A, W),
		    Target= F/A
		),
		format_to_string("@tt{~q}", [Target], DeclDS)
	    ),
	    DeclDSL
	),
	( DeclDSL=[]
	-> true
	; to_csl(DeclDSL) / (CSL-[]),
	    format_to_string("Declarations (~w):", [Type], Label),
	    add_rows(Label, [CSL]) / (penv, table)
	).
*/
% ......................................................................

gen_operators / (penv, table) :-
	findall(
	    OpDS,
	    (
		xml_content('module-usage', env(operator, W, _)),
		member_nb(precedence=Prec, W),
		member_nb(type=Type,       W),
		member_nb(functor=Functor, W),
		format_to_string("@tt{op( ~w, ~w, ~q)}",
		    [Prec, Type, Functor], OpDS)
	    ),
	    OpDSL
	),
	( OpDSL=[]
	-> true
	; to_csl(OpDSL) / (CSL-[]),
	    add_rows("Operators:", [CSL]) / (penv, table)
	).

% ======================================================================

gen_document_exports / (penv, body) :-
	findall(
	    Pred,
	    ( xml_content('predicates', Pred),
		Pred= env(predicate, _, _) ),
	    Preds
	),
	( Preds=[]
	-> true
	; [env(h2, [], ["Documentation on Exports"])] / body,
	    document_exports(Preds) / (penv, body)
	).

% ......................................................................

document_exports([]) / (penv, body) :- true.
document_exports([Pred|L]) / (penv, body) :-
	( document_pred(Pred) / (penv, body)
	-> true
	; true
	),
	document_exports(L) / (penv, body).

% ......................................................................

document_pred(env(_, W, Content)) / (penv, body) :-
	member_nb(functor=F, W),
	member_nb(arity=A,   W),
	member_nb(type=Type, W),
	format_to_string(" (~q)", [Type], TypeS),
	format_to_string("~q",    [F/A],  Sig),
	pred_anchor(Sig, Anchor),
	[env(a, [name=Anchor], [])] / body,
	[env(table, [class=doc_pred_table],
		[
		    env(tr, [class=doc_pred_head],
			[
			    env(th, [colspan=2],
				[env(span, [class=doc_pred_name], [Sig]),
				    env(span, [class=doc_pred_type], [TypeS])
				])
			])|Rows
		])
	] / body,
	document_pred_rows(F, A, Content) / (penv, Rows-[]).

% ......................................................................

document_pred_rows(_F, _A, Content) / (penv, table) :-
	( member_nb(env(comment, _, [CommentDS]), Content) ->
%	    add_rows("Comment:", [CommentDS]) / (penv, table)
	    add_postproc_content / (+CommentDS, penv, +Comment, -_),
	    [env(tr, [], [env(td, [colspan=2], Comment)])] / table
	; true
	),
	findall(
	    Properties,
	    member(env(properties, _, Properties), Content),
	    AllProperties
	),
	document_all_properties(AllProperties) / (penv, table).

% ......................................................................

document_all_properties([]) / (penv, table) :- true.
document_all_properties([Properties|L]) / (penv, table) :-
	document_property_set(Properties) / (penv, table),
	document_all_properties(L) / (penv, table).

% ......................................................................

document_property_set(PropContent) / (penv, table) :-
	( member_nb(env(head, W, [HDTXT]), PropContent)
	-> format_to_string("@bf{@tt{~s}}", [HDTXT], HDDS0),
	    ( member_nb(standard=iso, W)
	    -> format_to_string("~s @iso{}", [HDDS0], HDDS)
	    ; HDDS= HDDS0
	    ),
	    add_rows("Usage:", [HDDS]) / (penv, table)
	; true
	),
	( member_nb(env(description, _, [DS]), PropContent)
	-> add_rows("Description:", [DS]) / (penv, table)
	; true
	),
	findall(
	    PropGroup,
	    ( PropGroup= env('prop-group', _, _),
		member(PropGroup, PropContent)
	    ),
	    PropGroups
	),
	document_property_groups(PropGroups) / (penv, table).

% ......................................................................

document_property_groups([]) / (penv, table) :- true.
document_property_groups([PG|L]) / (penv, table) :-
	( document_property_group(PG) / (penv, NewDS-[]) ->
%	    add_rows("Properties:", [NewDS]) / (penv, table)
	    % JF: just testing, row label seems superfluous 
	    add_rows("", [NewDS]) / (penv, table)
	; true
	),
	document_property_groups(L) / (penv, table).

% ......................................................................

document_property_group(env('prop-group', W, Props)) / (penv, text) :-
	( member_nb(text=TextA, W)
	-> format_to_string("@em{~w}\n\n", [TextA], Text),
	    prefix(Text) / text
	; true
	),
	"@begin{itemize}" / text,
	document_properties(Props) / (penv, text),
	"\n@end{itemize}" / text.

% ----------------------------------------------------------------------

document_properties([]) / (penv, text) :- true.
document_properties([env(property, W, PContent)|L]) / (penv, text) :-
	( member_nb(module=PModule, W),
	    member_nb(functor=PF, W),
	    member_nb(arity=PA,   W),
	    "\n@item " / text,
	    document_property(PF/PA, PModule, PContent) / (penv, text)
	-> true
	; true
	),
	document_properties(L) / (penv, text).

% ......................................................................

document_property(PF/PA, PModule, PContent) / (penv, para) :-
	( member_nb(env(description, _, [DS]), PContent)
	-> prefix(DS) / para
	; member_nb(env(form, _, [Form]), PContent),
	    "@tt{" / para,
	    prefix(Form) / para,
	    "}" / para
	; format_to_string("~q", [PF/PA], DS),
	    prefix(DS) / para
	),
	( member_nb(env(heading, _, [Label]), PContent) %,
%	   append( _, [32, 0'a, 0't, 32 | Txt1], Heading)
	-> true %format_to_string( "At ~s", [Txt1], Label)
	; Label= "Property:"
	),
	" [@tt{" / para,
	format_to_string("~w", [PModule:PF/PA], PModuleS),
	prefix(PModuleS) / para,
	"}]" / para.
