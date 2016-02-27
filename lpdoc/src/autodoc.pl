:- module(autodoc, [], [dcg, assertions, regtypes, basicmodes, fsyntax]).

:- doc(title, "Documentation Generation Library").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- doc(module, "
   @cindex{automatic documentation library}

   This library provides some predicates which generate documentation
   automatically for a given module or application, using the
   declarations and assertions used in the module itself as input (see
   the @lib{assertions} library). By default, only the
   @concept{exported predicates} of the module appear in the
   documentation.  The predicates will be documented in the order in
   which they appear in the @pred{module/1} or @pred{module/2}
   declaration. @cindex{module declaration} 

   @cindex{automatic documentation}

   The idea of this package is on one hand to reuse the information
   present in the assertions and on the other to help ensure that code
   and documentation are kept as coherent as possible.  Hopefully,
   keeping them close together should help in this always difficult
   task.  The resulting documentation is somewhat rigidly structured,
   but generally sufficient for a @em{reference} manual, provided a
   little effort is put into the assertions and comments. The end
   product understandably depends heavily on how much work is put into
   adding additional comments to the source. Some documentation will
   be generated in any case, but it is recommended that, at the
   minimum, a module title and a comment for each of the exported
   predicates be provided.

@begin{alert}   
@bf{Note: This part is obsolete. -- JFMC}
@end{alert}

   The output format @cindex{documentation format} in which the
   documentation is generated is defined by the backend modules
   (@lib{autodoc_texinfo}, @lib{autodoc_html}, @lib{autodoc_man},
   etc.).

   The main output format supported is @tt{texinfo} (see The GNU
   Texinfo Documentation System manual for more info), from which
   printed manuals and several other printing and on-line formats can
   be easily generated automatically (including info, html,
   etc.). There is also some limited support for direct output in unix
   @tt{man} format and direct @tt{html} (but note that html can also
   be generated in a better way by first generating texinfo and then
   using one of the available converters). For texinfo, the
   documentation for a module is a texinfo chapter, suitable for
   inclusion in a wrapper ``main'' document file.  A simple example of
   the use of this library for generating a texinfo reference manual
   (including a driver script, useful Makefiles, etc.) is included
   with the library source code. Other examples can be found in the
   Ciao documentation directory (i.e., the Ciao manuals themselves).

   A simple example of the use of this library for generating a
   @tt{texinfo} @cindex{texinfo} reference manual (including a driver
   script, useful Makefiles, etc.) is included with the library source
   code. Other examples can be found in the Ciao documentation
   directory (i.e., the Ciao manuals themselves).
").

% ---------------------------------------------------------------------------

%% Order below is still important (at least in current Ciao version):

:- doc(hide, library_directory/1).

:- multifile library_directory/1.
:- dynamic library_directory/1.

% ISO-Prolog compatibility libraries
:- use_module(library(ttyout)).
:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(read),          [read/2]).
:- use_module(library(make(make_rt)), [verbose_message/1, verbose_message/2]).
:- use_module(library(dict)).

% Ciao libraries
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(assertions(assrt_lib)),
	    [
		cleanup_code_and_related_assertions/0,
		clause_read/7,
		assertion_read/9,
		assertion_body/7,
		get_code_and_related_assertions_opts/6,
		set_libs/2
	    ]).
:- use_module(library(compiler(c_itf))).
:- use_module(library(assertions(assertions_props)),
	    [predfunctor/1, propfunctor/1]).
:- use_module(library(messages)).
:- use_module(library(filenames), [no_path_file_name/2, basename/2]).
:- use_module(library(lists),
	    [append/3, reverse/2, length/2, list_concat/2, select/3]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(make(system_extra)), [(-) /1, try_finally/3]).

% Local libraries
:- use_module(lpdocsrc(src(autodoc_settings))).
:- use_module(lpdocsrc(src(autodoc_filesystem))).
:- use_module(lpdocsrc(src(autodoc_structure))).
:- use_module(lpdocsrc(src(autodoc_doctree))).
:- use_module(lpdocsrc(src(autodoc_refsdb))).
:- use_module(lpdocsrc(src(autodoc_parse))).
:- use_module(lpdocsrc(src(autodoc_index))).
:- use_module(lpdocsrc(src(comments)), [version_descriptor/1, docstring/1,
	stringcommand/1, doc_id_type/3]).

% ===========================================================================

:- doc(section, "Definitions for Backends").

% TODO: (This should be defined in the backend -- indeed is the backend list)
:- export(backend_id/1).
:- regtype backend_id(Id) # "@var{Id} is a supported backend.".

backend_id(texinfo).
backend_id(html).
backend_id(man).

% TODO: (This should be defined in the backend)
:- export(backend_ignores_components/1).
:- pred backend_ignores_components(Id) # "@var{Id} does not take into
   account components (only documents the @em{mainfile})".

backend_ignores_components(man).

:- export(backend_alt_format/2).
:- pred backend_alt_format(Id, Ext) # "@var{Ext} is an alternative
   file format that can be generated by the backend @var{Id}".

backend_alt_format(texinfo, dvi).
backend_alt_format(texinfo, ps).
backend_alt_format(texinfo, pdf).
backend_alt_format(texinfo, info).
backend_alt_format(texinfo, ascii).
backend_alt_format(texinfo, rtf). % TODO: obsolete? working?

% TODO: Obtain from the backend
:- export(top_suffix/2).
:- pred top_suffix(FileFormat, PrincipalExt) # "@var{PrincipalExt} is
   extension of the target file that will generate the file with
   @var{FileFormat} extension.".

top_suffix('html', 'htmlmeta') :- !.
top_suffix(Suffix, Suffix).

%% ===========================================================================

:- export(index_comment/2).
:- pred index_comment(Index,Text) 

	=> atom * string

        # "@var{Type} is a type of index which is
          supported. @var{Text} describes the index contents.".

index_comment(Type,Text) :-
	typeindex(Type,_,_,Text,_).

:- use_module(lpdocsrc(src(autodoc_doctree))).

%% ---------------------------------------------------------------------------

:- doc(section, "Documentation Options").

:- export(supported_option/1).
:- prop supported_option(Option) 

	# "@tt{Option} is a supported documentation option.".

supported_option('-v').
supported_option('-nobugs').
supported_option('-noauthors').
supported_option('-noversion').
supported_option('-nochangelog').
supported_option('-nopatches').
supported_option('-modes').
supported_option('-headprops').
supported_option('-literalprops').
supported_option('-nopropnames').
supported_option('-noundefined').
supported_option('-nopropsepln').
supported_option('-nobiblio').
supported_option('-nosysmods').
supported_option('-noengmods').
supported_option('-noisoline').
supported_option('-propmods').
supported_option('-nopropuses').
supported_option('-shorttoc').
supported_option('-regtypeprops').
supported_option('-onesided').
supported_option('-nomath').

:- export(option_comment/2).
:- pred option_comment(Option,Text) 

	=> supported_option * string

        # "@var{Option} is a documentation option which is
          supported. @var{Text} describes the effect of selecting that
          option. Currently supported options are:

@includedef{option_comment/2}          
          ".

option_comment('-v',           "Verbose output (good for debugging).        ").
option_comment('-nobugs',      "Do not include information on bugs.         ").
option_comment('-noauthors',   "Do not include author names.                ").
option_comment('-noversion',   "Do not include version information.         ").
option_comment('-nochangelog', "Do not include change log.                  ").
option_comment('-nopatches',   "Do not include comments for patches.        ").
%% -modes and -headprops are used by normalizer!
option_comment('-modes',       "Do not translate modes and their arguments
	                        (except for properties)                     ").
option_comment('-headprops',   "Do not move head properties to body.        ").

option_comment('-literalprops',"Do not use text to document properties.     ").
option_comment('-nopropnames', "Do not include property names in prop text. ").
option_comment('-noundefined', "Do not signal undefined properties in text. ").
option_comment('-nopropsepln', "Do not put each property in a separate line.").

option_comment('-nobiblio',    "Do not include a bibliographical 'References' appendix.").
option_comment('-nosysmods',   "Do not include system modules in list of 
                                libraries used.").
option_comment('-noengmods',   "Do not include system engine modules in list 
                                of libraries used.").
option_comment('-noisoline',   "Do not include *textual* description that a 
                                given usage conforms to the ISO standard.").
option_comment('-propmods',    "Include module name to which props belong.").
option_comment('-nopropuses', "Do not Include property uses (from assertions) in indices.").
option_comment('-shorttoc',    "Produce shorter table of contents (no entries
                                for individual defs of preds, props, etc.).").
option_comment('-regtypeprops',"Include in the doc for regtypes the global
                                prop stating that they are indeed regtypes.").
option_comment('-onesided',    "For printing on one side (default is two).").
option_comment('-nomath',      "Disable mathematical environments.").

% ===========================================================================

:- doc(section, "Documentation State for a Module").
% This is the (partial) state of the generation process for the
% current component.

:- use_module(library(dict)).

% TODO: refine
:- export(docstate/1).
:- regtype docstate/1.
docstate(docstate(Backend, Name, Opts, CustomDic, I)) :-
	backend_id(Backend),
	atom(Name),
	list(Opts,supported_option),
	dictionary(CustomDic), % to keep custom data
	filename(I).

:- export(docstate_backend/2).
docstate_backend(DocSt, Backend) :-
	DocSt = docstate(Backend, _, _, _, _).

:- export(docstate_currmod/2).
docstate_currmod(DocSt, Name) :-
	DocSt = docstate(_, Name, _, _, _).

:- export(docstate_set_currmod/3).
docstate_set_currmod(DocSt0, Name, DocSt) :-
	DocSt0 = docstate(Backend, _,    Opts, CustomDic, I),
	DocSt  = docstate(Backend, Name, Opts, CustomDic, I).

:- export(docstate_opts/2).
docstate_opts(DocSt, Opts) :-
	DocSt = docstate(_, _, Opts, _, _).

:- export(docstate_set_opts/3).
docstate_set_opts(DocSt0, Opts, DocSt) :-
	DocSt0 = docstate(Backend, Name, _,    CustomDic, I),
	DocSt  = docstate(Backend, Name, Opts, CustomDic, I).

%:- export(docstate_customdic/2).
docstate_customdic(DocSt, CustomDic) :-
	DocSt = docstate(_, _, _, CustomDic, _).

:- export(docstate_set_customdic/3).
docstate_set_customdic(DocSt0, CustomDic, DocSt) :-
	DocSt0 = docstate(Backend, Name, Opts, _, I),
	DocSt  = docstate(Backend, Name, Opts, CustomDic, I).

:- export(docstate_inputfile/2).
docstate_inputfile(DocSt, I) :-
	DocSt = docstate(_, _, _, _, I).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Create a New docstate").

% Create a new docstate (with no source loaded)
% TODO: keep OldLibs! remember to reset them
new_docstate_no_src(Backend, Name, Opts, DocSt) :-
	DocSt = docstate(Backend, Name, Opts, _, ''),
	%
        setup_libpaths(DocSt, _). % TODO: Necessary?

% Create a new docstate and load the source
new_docstate_with_src(Backend, FileBase, SourceSuffix, Opts, DocSt, OldLibs) :-
	DocSt = docstate(Backend, Name, Opts, _, I),
	%
	setup_libpaths(DocSt, OldLibs),
	%
	( SourceSuffix = 'lpdoc' ->
	    % TODO: incomplete and probably incorrect
	    I = '',
	    get_name(FileBase, Name),
	    doc_customdic_lookup(DocSt, modinfo, modinfo(Name, FileBase))
	; load_source(FileBase, SourceSuffix,
	    Name, M, I, Base, Dir,
	    Opts),
	  % TODO: M (the Prolog module name) and Name may be different...
	  doc_customdic_lookup(DocSt, modinfo, modinfo(M, Base)),
	  doc_customdic_lookup(DocSt, dir, dir(Dir))
	).

%% ---------------------------------------------------------------------------

:- pred load_source(FileBase, SourceSuffix,
	    Name, M, I, Base, Dir,
	    Opts)

# "Main file processing routine. Reads code and assertions, opens
   output files, etc. Also eliminates any assertions that come from
   the assertions package -- except when documenting the assertions
   package itself, of course.

   @var{FileBase}: input file name without suffix.
   @var{SourceSuffix}: suffix of the input file name.

   @var{Name}: simple input file name (no dir, no suffix).
   @var{M}: defined module (or user(file)).  @var{I}:
   full input file name (with dir and suffix).  @var{Base}: full input
   file name (with dir but no suffix). @var{Dir}: full directory
   path.@var{Opts}: options.
".

%% The assertions package is treated normally
load_source(FileBase, SourceSuffix,
	    % Output vars
	    Name, M, I, Base, Dir,
	    %
	    Opts) :-
	% optional_mess("Gathering and normalizing assertions from T"),
	%
        atom_concat([FileBase, '.', SourceSuffix], Main),
	load_source_pl_assrt(Main, Opts, M, Base, Dir, I),
	atom_concat([Dir,        '/', Name],   Base).
	% TODO: I removed this, since AssrtOps is always []
%% 	( includes(Base, library(assertions)) -> %% possibly only 'assertions'
%% 	    ( member(Op, AssrtOps),
%% 		retractall_fact(clause_read(_, 1, Op, _, _, _, _)),
%% 		fail
%% 	    ; true
%%             )
%% %	    findall(_,(member(Op,AssrtOps),
%% %	    retract_fact(clause_read(_,1,Op,_,_,_,_))),_),
%% %	    findall(_,(member(NDP,AssrtNDPs),
%% %	    clause_read(_,1,new_declaration(NDP),_,_,_,_)),_) 
%% 	;
%% 	    true
%% 	),
%%% No good, for now they are indistiguishable...
%%% Eliminate clauses coming from library(assertions) unless Main=assertions!
%% 	(  Main = assertions
%% 	-> true
%% 	;  simple_message("*** Eliminating assertions stuff..."),
%% 	   base_name(library(assertions),AssrtBase),
%% 	   simple_message("*** AssrtBase is ~w",[AssrtBase]),
%% 	   findall(_,
%% 	           ( clause_read(AB,A,B,C,D,E,F),
%%      	             simple_message("*** retracting ~w",
%%                            [clause_read(AB,A,B,C,D,E,F)]) ), _)
%% 	),

load_source_pl_assrt(Main, Opts,
	    % Output vars
	    M, Base, Dir, I) :-
	absolute_file_name(library(Main), I),
	%
	cleanup_c_itf_data,
	cleanup_code_and_related_assertions,
	prolog_flag(quiet, _, off),
	get_code_and_related_assertions_opts(I, Opts, M, Base, _Suffix, Dir).

% TODO: many callers of this predicate do not restore the paths later
setup_libpaths(DocSt, OldLibPaths) :-
	doc_opt('-lib-opts'(LibPaths, SysLibPaths, PathAliasF), DocSt),
	append(LibPaths, SysLibPaths, TheLibPaths),
	set_libs(OldLibPaths, TheLibPaths),
	( PathAliasF = [ThePathAliasFile] ->
	    use_module(library(ThePathAliasFile))
	; true
	).
%% Keep: we may need the paths later reading files (e.g., includes...)
%%	set_libs(_,OldLibPaths),

% ---------------------------------------------------------------------------

:- doc(subsection, "Common Operations on a docstate").

:- use_module(library(messages)).

:- export(doc_message/2).
doc_message(Text, DocSt) :-
	docstate_opts(DocSt, Opts),
	optional_message(Text, Opts).

:- export(doc_message/3).
doc_message(Text, Args, DocSt) :-
	docstate_opts(DocSt, Opts),
	optional_message(Text, Args, Opts).

:- export(doc_opt/2).
doc_opt(Opt, DocSt) :-
	docstate_opts(DocSt, Opts),
	member(Opt, Opts), !.

:- use_module(library(lists), [select/3]).

:- export(doc_customdic_lookup/3).
doc_customdic_lookup(DocSt, K, V) :-
	docstate_customdic(DocSt, CustomDic),
	dic_lookup(CustomDic, K, V).

:- export(doc_customdic_replace/4).
doc_customdic_replace(DocSt0, K, V, DocSt) :-
	docstate_customdic(DocSt0, CustomDic0),
	dic_replace(CustomDic0, K, V, CustomDic),
	docstate_set_customdic(DocSt0, CustomDic, DocSt).

:- export(doc_customdic_get/3).
doc_customdic_get(DocSt, K, V) :-
	docstate_customdic(DocSt, CustomDic),
	dic_get(CustomDic, K, V).

:- export(doc_customdic_maybe_get/3).
doc_customdic_maybe_get(DocSt, K, V) :-
	( doc_customdic_get(DocSt, K, V0) ->
	    V = V0
	; V = []
	).

:- export(doc_doing_mainmod/1).
doc_doing_mainmod(DocSt) :-
	docstate_currmod(DocSt, Name),
	get_mainmod(Name), !.

% :- export(doc_no_components/1).
doc_no_components(_DocSt) :- all_component_specs([]).

:- export(doc_modname/2).
:- pred doc_modname(DocSt, ModName) # "@var{ModName} is the name of
   the module that we are documenting.".
doc_modname(DocSt, NDName) :-
	docstate_currmod(DocSt, Name),
	( atom_concat(NDName, '_doc', Name) ->
	    true
	; NDName = Name
	).

% ----------------------------------------------------------------------

:- doc(subsection, "Required Documentation Indices").
% (for a given backend and the current options)

:- use_module(library(aggregates), [findall/3]).

:- export(docstate_has_index/2).
docstate_has_index(Index, DocSt) :-
        doc_opt('-indices'(Indices), DocSt),
	( member(Index, Indices) -> true
	; Indices=[all]
	).

:- export(all_indices/2).
all_indices(DocSt, Indices) :-
	findall(I, enum_indices(I, DocSt), Indices).

% Enumerate (backtracking) the indices in the order stablished by
% typeindex.
enum_indices(IdxName, DocSt) :-
	typeindex(IdxName, _Index, _IType, _ITitle, _IComment),
	docstate_has_index(IdxName, DocSt).

% ---------------------------------------------------------------------------

:- doc(subsection, "Querying the Module Loaded in docstate").

:- export(get_doc/4).
% :- pred get_doc(in(Id),in(MessageType),in(DocState),go(Comment)).
% Obtain the value of document property Id.
% If the value is not defined, the action specified by MessageType is carried out.

get_doc(Id, MessageType, DocSt, Value) :-
	doc_id_type(Id, Type, ValueType),
	get_doc__2(Id, Type, ValueType, MessageType, DocSt, Value).

get_doc__2(Id, single, ValueType, _MessageType, DocSt, Value) :-
	get_doc_field(Id, RContent, Loc),
	!,
	process_content(ValueType, DocSt, Loc, RContent, Value).
get_doc__2(Id, multiple, ValueType, _MessageType, DocSt, Values) :-
	findall(Value,
	    ( get_doc_field(Id, RContent, Loc),
	      process_content(ValueType, DocSt, Loc, RContent, Value) 
            ),
	    Values),
	Values \== [],
	!.
get_doc__2(Id, Type, ValueType, MessageType, DocSt, Value) :-
	( Type = single, ValueType = docstr ->
	    % default empty case
	    empty_doctree(Value)
	; Value = []
	),
	docstate_inputfile(DocSt, S),
	show_message_type(MessageType, loc(S, 1, 1),
	    "no "":- doc(~w,...)"" declaration found", [Id]).

process_content(ValueType, DocSt, Loc, RContent, Value) :-
	( ValueType = docstr ->
	    % Parse a docstring to obtain a doctree
	    % TODO: emit error if \+string(RContent)
	    parse_docstring_loc(DocSt, Loc, RContent, Value)
	; Value = RContent
	).

%% The lowest message levels (for the options in get_doc_field)

show_message_type(ignore,      _,   _,      _) :- !.
show_message_type(dofail,      _,   _,      _) :- !, fail.
show_message_type(MessageType, Loc, Format, Args) :-
	show_message(MessageType, Loc, Format, Args).

get_doc_field(Id0, Field, loc(S, LB, LE)) :-
%	(Id=title->display('b:'),display(Id),display(':'),display(Field),nl;true),
	( Id0 = pred(Id) -> true ; Id = Id0 ),
	% TODO: accept 'comment' for compatibility, but emit deprecate message
	( clause_read(_, 1, comment(Id, Field), Dict, S, LB, LE)
	; clause_read(_, 1, doc(Id, Field), Dict, S, LB, LE)
	),
	bind_dict_varnames(Dict).

get_doc_field_dict(Id0, Field, Dict) :-
	% TODO: accept 'comment' for compatibility, but emit deprecate message
	( Id0 = pred(Id) -> true ; Id = Id0 ),
	( clause_read(_, 1, comment(Id, Field), Dict, _, _, _)
	; clause_read(_, 1, doc(Id, Field), Dict, _, _, _)
	).

:- export(modtype/1).
:- regtype modtype/1 # "Represents the type of file being documented.".
:- doc(modtype/1, "@includedef{modtype/1}").

modtype(part). % (introduction of a part)
modtype(application).
modtype(module).
modtype(user).
modtype(include).
modtype(package).

% :- export(detect_modtype/2).
:- pred detect_modtype(DocSt, FileType) => docstate * modtype.
detect_modtype(DocSt, FileType) :-
	get_doc(filetype, dofail, DocSt, FileType0),
	!,
	( modtype(FileType0) ->
	    FileType = FileType0
	; error_message("Unrecognized value in doc(filetype) declaration"),
	  fail % TODO: recover from this error?
	).
%% Application - no interface, so no complication
detect_modtype(DocSt, FileType) :-
	doc_customdic_get(DocSt, modinfo, modinfo(_, Base)),
	( defines(Base, main, 0, _, _)
	; defines(Base, main, 1, _, _)
	),
	!,
	FileType = application.
%% Else, we need to infer the type
detect_modtype(DocSt, FileType) :-
	doc_customdic_get(DocSt, modinfo, modinfo(M, _)),
	( M = user(_) ->
	    FileType = package % TODO: This is wrong, check for ":- package" declarations instead
	; FileType = module
	).

% :- export(get_first_loc_for_pred/3).
%%% BUG: NEED to check for loops!
get_first_loc_for_pred(F, A, loc(S, L0, L1)) :-
	functor(Head, F, A),
	clause_read(_, Head, _Body, _VarNames, S, L0, L1),
	!.
get_first_loc_for_pred(F, A, loc(S, L0, L1)) :-
	clause_read(_, 1, multifile(F/A), _, S, L0, L1),
	!.
get_first_loc_for_pred(_, _, _).

% Like get_doc, but emits 'note' errors if doing mainmod
get_mod_doc(P, DocSt, Value) :-
	( doc_doing_mainmod(DocSt) ->
	    ErrorType = note
	; ErrorType = ignore
	),
	get_doc(P, ErrorType, DocSt, Value).

% ---------------------------------------------------------------------------

:- doc(section, "Output Directory Preparation").
% Make sure that the output directory for this target has been prepared
% TODO: move customization or options to each backend?

:- data output_dir_prepared/1.

:- export(reset_output_dir_db/0).
reset_output_dir_db :-
	retractall_fact(output_dir_prepared(_)).

:- export(ensure_output_dir_prepared/2).
:- pred ensure_output_dir_prepared(Backend, Opts) # "Ensure that the
   output directories for backend @var{Backend} are prepared.".
% Prepare the output directory (computes the output directory, makes
% sure it exists, and calls @pred{prepare_auxfiles/2}).
ensure_output_dir_prepared(Backend, _) :-
	current_fact(output_dir_prepared(Backend)),
	!.
ensure_output_dir_prepared(Backend, Opts) :-
	assertz_fact(output_dir_prepared(Backend)),
	% Make sure that the output directory exists
	ensure_output_dir(Backend),
	%
	prepare_auxfiles(Backend, Opts).

% Copy the auxiliary files required for this output. This is specific
% to each backend (e.g. CSS, web-site skeleton, images, etc.)
%
prepare_auxfiles(Backend, Opts) :- Backend = html, !,
	( setting_value(website_skeleton, SkelDir) ->
	    prepare_web_skel(SkelDir)
	; true
	),
	( member('-nomath', Opts) ->
	    true
	; prepare_mathjax
	),
	% Add CSS files
	CSS = 'lpdoc.css',
	setting_value(lpdoclib, LpdocLib),
	atom_concat(LpdocLib, CSS, HtmlStyle),
	absfile_for_aux(CSS, Backend, OutCSS),
	copy_file(HtmlStyle, OutCSS, [overwrite]).
prepare_auxfiles(_, _) :- !.

:- use_module(library(make(system_extra)), [copy_file/3]).

:- use_module(lpdocsrc(src(autodoc_html_resources)),
	[prepare_web_skel/1, prepare_mathjax/0]).

% ===========================================================================

% TODO: Add type to Opts (indices, lib-opts and paper-opts are missing!)
:- export(get_autodoc_opts/3).
:- pred get_autodoc_opts(Backend, Mod, Opts) : atm * atm * list(supported_option) #
   "Get the list of documentation options @var{Opts} for the
   @var{FileBase} file.".

get_autodoc_opts(_Backend, Mod, Opts) :-
	StartPage = ~setting_value_or_default(startpage),
	PaperType = ~setting_value_or_default(papertype),
	Libs = ~all_setting_values(filepath),
	SysLibs = ~all_setting_values(systempath),
	Indices = ~all_setting_values(index),
	PathAliasF = ~all_setting_values(pathsfile),
	%
	( Mod = ~get_mainmod ->
	    Opts0 = ~all_setting_values(doc_mainopts),
	    % TODO: Should this be here?
	    % Complain about not defined values (only when documenting mainmod)
	    warn_if_empty(Libs, "no filepath variable was found"),
	    warn_if_empty(SysLibs, "no systempath variable was found"),
	    warn_if_empty(Indices, "no index variable was found"),
	    warn_if_empty(PathAliasF, "no pathsfile variable was found")
	; Opts0 = ~all_setting_values(doc_compopts)
	),
	Opts = ['-indices'(Indices),
	        '-lib-opts'(Libs, SysLibs, PathAliasF),
	        '-paper-opts'(StartPage, PaperType)|Opts0].

warn_if_empty(X, Msg) :-
	( X == [] ->
	    verbose_message("~s", [Msg])
	; true
	).

% ===========================================================================

:- doc(section, "Generate and Save the doctree for a Module").

:- export(autodoc_gen_doctree/5).
:- pred autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Mod)
	:: backend_id * basename * atm * list(supported_option) * atm

# "@var{FileBase} is the module specifier of the source file being
   documented (without extension, @var{SourceSuffix} is the suffix of
   the source). The output is a file whose contents document the main
   file, based on any assertions present in that file.  The
   documentation is produced in the format given by @var{Backend} (the
   name of the output file also depends on @var{Backend}). The formats
   supported are given by @pred{backend_id/1}. @cindex{supported
   documentation formats}".

% '-indices'(Indices)
% '-lib-opts'(LibPaths, SysLibPaths, PathAliasF)
% '-paper-opts'(StartPage, PaperType)
%
%      @var{LibPaths} is a list of @concept{library paths} where files
%      used by the module being documented may be
%      found. @var{SysLibPaths} is similar to @var{LibPaths} but
%      provides paths to @em{system} libraries. @var{PathAliasF} is the
%      name of a module containing path aliases. @var{Indices} is a list
%      of index names (the @concept{indices generated
%      automatically}). @var{StartPage} is the page number of the first
%      page of the manual. This can be useful if the manual is to be
%      included in a larger document or set of manuals.   

% Note on performance:
%   For the average module, doctree scan and write is fast. The
%   slowest part is reading the source file.
%
% TODO: Profile the source reader; time may be wasted looking at
%       dependencies, whose cached information is possibly discarded
%       from one execution to the other. --JF

autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Mod) :-
	verbose_message("{Generating ~w documentation for ~w", [Backend, FileBase]),
	new_docstate_with_src(Backend, FileBase, SourceSuffix, Opts, DocSt, OldLibs),
	%
	( SourceSuffix = 'lpdoc' ->
	    GlobalVers = [],
	    TitleR = [],
	    % TODO: This is a really simple and incomplete version for .lpdoc as a source!
	    atom_codes(FileBase, FBC),
	    append("@include{"|| FBC, ".lpdoc}", Command),
	    parse_docstring_loc(DocSt, _Loc, Command, ContentR),
	    ModuleR = [% TODO: first chapter line is not backend-agnostic, fix
		       raw("@chapter "), raw(FBC), raw("\n\n"),
		       ContentR]
	; detect_modtype(DocSt, ModuleType),
	  doc_message("File being documented as '~w'", [ModuleType], DocSt),
	  ( ModuleType = part ->
	      Version = [], GlobalVers = []
	  ; get_last_version(Version, GlobalVers, DocSt)
	  ),
	  %
	  ( Backend = man ->
	      % Generates a brief description of the application or library in
	      % @concept{unix man format}.
	      ( ModuleType = application ->
	          % TODO: Should 'S' be 'I' here?
	      	  ( clause_read(_, usage_message(_), true, _, S, LB, LE) ->
	      	      UsageString = "@begin{verbatim}@includefact{usage_message/1}@end{verbatim}",
	      	      parse_docstring_loc(DocSt, loc(S, LB, LE), UsageString, UsageR)
	      	  ; note('No usage_message/1 fact found for application'),
	      	    UsageR = []
	          )
	      ; UsageR = []
	      ),
	      get_doc(title, warning, DocSt, TitleR),
	      get_authors(DocSt, AuthorRs),
	      get_mod_doc(copyright, DocSt, CopyrightR),
	      get_mod_doc(summary,   DocSt, SummaryR),
	      ModuleR = man_page(TitleR, GlobalVers, AuthorRs, SummaryR, UsageR, CopyrightR)
	  ; % The general case
	    doc_message("Generating front matter and intro...", DocSt),
	    %
	    fmt_module(DocSt, ModuleType, Version, GlobalVers, ModuleR)
	  )
	),
	% Register the title and version of the main file
	register_main_title(Version, DocSt),
	%
	doctree_scan_and_save(ModuleR, Mod, DocSt),
	% TODO: This generates the infoindex if necessary; generalize for other formats
	( %SourceSuffix = pl,
	  doc_doing_mainmod(DocSt),
	  Backend = texinfo ->
	    fmt_infodir_entry(DocSt, GlobalVers, Mod)
	; true
	),
	set_libs(_, OldLibs),
	%
	verbose_message("}"),
	ttyflush,
	!.
autodoc_gen_doctree(_, FileBase, _, _, _) :-
	error_message("formatting ~w could not be completed", [FileBase]).

doctree_scan_and_save(R, Mod, DocSt) :-
	% Scan the references, save them, and save the doctree.
	doctree_scan_and_save_refs(R, DocSt),
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(Mod, Backend, dr, RFile),
	doctree_save(RFile, R).

get_last_version(Version, GlobalVers, DocSt) :-
	( doc_opt('-noversion', DocSt) ->
	    Version = [], GlobalVers = []
	; doc_customdic_get(DocSt, dir, dir(Dir)),
	  get_last_version_(Version, GlobalVers, Dir, DocSt)
	).

register_main_title(Version, DocSt) :-
	get_doc(title, ignore, DocSt, TitleR),
	( doc_doing_mainmod(DocSt) ->
	    MainTitleR = [TitleR|MainTitleR0],
	    ( version_numstr(Version, VerStr) ->
	        MainTitleR0 = [string_esc(" v"), string_esc(VerStr)]
	    ; MainTitleR0 = []
	    ),
	    add_refs_entry(main_title(MainTitleR), DocSt)
	; true
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Format Authors").

get_authors(DocSt, AuthorRs) :-
	( doc_opt('-noauthors', DocSt) ->
	    AuthorRs=[]
	; get_doc(author, warning, DocSt, AuthorRs0),
	  ( doc_doing_mainmod(DocSt) ->
	      % Do not add defauthor in this case
	      % (this is the cover!)
	      AuthorRs1 = AuthorRs0
	  ; add_author_defs(AuthorRs0, DocSt, AuthorRs1)
	  ),
	  % TODO: good idea?
	  get_doc(credits, ignore, DocSt, CreditsRs),
	  append(AuthorRs1, CreditsRs, AuthorRs)
	).

add_author_defs([], _DocSt, []).
add_author_defs([A|As], DocSt, [B|Bs]) :-
	( doctree_to_rawtext(A, DocSt, A1),
	  remove_author_details(A1, Name0) ->
	    % TODO: special commands (like accents) are lost here!
	    Name = raw(Name0)
	; % do nothing, no extra info
	  Name = A
	),
	B = defauthor(local_label(_), Name, A),
	add_author_defs(As, DocSt, Bs).

% ---------------------------------------------------------------------------

:- doc(subsection, "Format infoindex").

% TODO: Generalize for other backends, not just info
:- pred fmt_infodir_entry(DocSt, Version, Mod)
	: docstate * term * basename
# "Generates a one line description (ASCII) of the application or library
   in a file for the directory of @tt{emacs info} manuals.".

fmt_infodir_entry(DocSt, Version, Mod) :-
	% TODO: Do not generate if not necessary (e.g. readme files...)
	verbose_message("{Generating info index", []),
	main_output_name(InfoBase),
	( doc_opt('-noversion', DocSt) ->
	    doc_modname(DocSt, NDName),
	    InfodirName = NDName
	; InfodirName = InfoBase % TODO: Not very nice...
	),
	atom_codes(InfodirName, InfodirNameS),
	atom_codes(InfoBase, InfoBaseS),
	%
	get_default_title(DocSt, TitleR),
	infodir_version(Version, VersionR),
	%
	R = [raw("* "), raw(InfodirNameS), raw(": ("), raw(InfoBaseS), raw(").\n\t"),
	     TitleR, VersionR, raw_nl],
	% Write the doctree contents of what will be in the '.infoindex' file
	% (note: see @pred{fmt_infodir_entry2} for more details)
	infodir_base(Mod, ModInfodir),
	%
	docstate_set_currmod(DocSt, ModInfodir, DocSt1),
	doctree_scan_and_save(R, ModInfodir, DocSt1),
	%
	verbose_message("}"),
	ttyflush.

:- export(infodir_base/2). % TODO: Temporally exported for lpdoc.pl
infodir_base(Mod, ModInfodir) :-
	atom_concat(Mod, 'dir', ModInfodir).

get_default_title(DocSt, TitleR) :-
	get_doc(title, ignore, DocSt, TitleR0),
	( is_nonempty_doctree(TitleR0) ->
	    TitleR = TitleR0
	; doc_modname(DocSt, NDName),
	  atom_codes(NDName, NDNameS),
	  TitleR = [raw(NDNameS), raw(" Reference Manual")]
	).

infodir_version(Version, VersionR) :-
	( version_date(Version, Date),
	  version_numstr(Version, VerStr)
	-> 
	    format_to_string("~w", [Date], DateStr),
	    VersionR = [raw(" (version "),
	                raw(VerStr), raw(" of "),
			raw(DateStr), raw(")")]
	; VersionR = []
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Obtain Version").

get_last_version_(Version, GlobalVers, Dir, DocSt) :-
	get_doc_field(version_maintenance, dir(VDir), Loc),
	!,
	%% version maintained in dir (computed relative to .pl file Dir!)
	atom_concat([Dir, '/', VDir, '/', 'GlobalChangeLog'], ChangeLogFile),
	doc_message("Getting global version from ~w...", [ChangeLogFile], DocSt),
	( file_exists(ChangeLogFile) ->
	    try_finally(
		open(ChangeLogFile, read, CLFS),
		read(CLFS, (:- doc(GlobalVers, _))),
		close(CLFS))
	; error_message(Loc,
		"Version file ~w not found, using version comments in file",
		[ChangeLogFile]),
	  GlobalVers = Version
	),
	get_last_local_version(Version, DocSt).
get_last_version_(Version, Version, _Dir, DocSt) :-
	%% else, component or version maintained in doc/2 decls in file
	get_last_local_version(Version, DocSt).

get_last_local_version(Version, DocSt) :-
	%% get last version in doc/2 decls in file
	doc_message("Getting local version from file...", DocSt),
	( setof(VTerm, version_field(VTerm), Versions),
	    %% Leaves most recent one last...
	    append(_, [LVersion], Versions)
	-> Version = LVersion
	;
	    ( lpdoc_option('-cv') ->
	        docstate_inputfile(DocSt, I),
		note_message(loc(I, 1, 1),
		    "no "":- doc(version(...),...)"" declaration found", []
		)
	    ; true
	    ),
	    Version = []
        ).

version_field(VTerm) :-
	get_doc_field(version(Version, Date), _Comment, _Loc),
	VTerm=version(Version, Date, []).
version_field(VTerm) :-
	get_doc_field(version(Version, Date, Time), _Comment, _Loc),
	VTerm=version(Version, Date, Time).

:- use_module(library(system), [file_exists/1]).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Module Name from Module Spec").

:- doc(main_filename(LongName, ShortName), "@var{ShortName} is the
	filename of @var{LongName}, without library path.").

:- pred main_filename(LongName, ShortName)
	:: string * string

# "@var{ShortName} is the filename of @var{LongName}, without
            library path.".

% TODO: Share with predicates in make_rt
main_filename(IS, NamePLS) :-
	filename_minus_libpath(IS, NamePLS),
	!.
main_filename(IS, NamePLS) :-
	name_after_last_slash(IS, NamePLS).

filename_minus_libpath(IS, NamePLS) :-
	library_directory(L),
	atom_codes(L, LS),
	append(LS, [0'/, NamePLS], IS).

name_after_last_slash(IS, NamePLS) :-
	reverse(IS, RIS),
	to_bar_or_end(RIS, RNamePLS),
	reverse(RNamePLS, NamePLS).

to_bar_or_end([], []) :- !.
to_bar_or_end([0'/|_], []) :- !.
to_bar_or_end([C|Cs], [C|Ss]) :-
	to_bar_or_end(Cs, Ss).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Document Module").

:- doc(
   fmt_module(
     DocSt,ModuleType,
     Version,GlobalVers,
     ModR),

   "This predicate defines the first part of the format of the main
   file of a manual, the introduction, and some auxiliary information.
%
   @var{TitleR} is the intended title of the application (taken from
   the approriate @pred{doc/2} declaration). 

   @begin{itemize}
   @item @var{Version} is the version of the first @pred{doc/2} entry
      which specifies a version number (which should be the current
      version). This is the version of the last local change. @var{GlobalVers}
      is the global version.
   @item @var{ModuleType} is type type of file.
   @end{itemize}
   ").

% @var{StartPage} is the page number of the first page of the manual.
%
% @var{Name} is the name of the application (taken from the name of
% the input file). @var{NDName} is the same, but without @tt{_doc},
% if applicable. 

:- pred fmt_module(
     DocSt,ModuleType,
     Version,GlobalVers,
     ModR)
        : docstate(DocSt), modtype(ModuleType),
	  version_descriptor(Version), version_descriptor(GlobalVers),
	  doctree(ModR).

fmt_module(DocSt, ModuleType,
	   Version, GlobalVers,
	   ModR) :-
        doc_doing_mainmod(DocSt),
	!,
	doc_message("Generating main documentation file", DocSt),
	%
	get_mod_doc(copyright, DocSt, CopyrightR),
	fmt_introduction(ModuleType, OtherR, DocSt, IntroR, MainRest),
	fmt_other_info(DocSt, OtherR),
	fmt_includes_and_biblio_and_indices(DocSt, EndR),
	%
	get_authors(DocSt, AuthorRs),
	get_coversec_prop(GlobalVers, AuthorRs, CopyrightR, DocSt, CoverProp),
	SecProps00 = [CoverProp, level(0)],
	%
	% TODO: add an option for this (does not really depend on the backend)
	( just_a_version(Version) ->
	    gen_version_note(Version, VersionR)
	; VersionR = nop
	),
	% The front page (not all backends have them)
	( docstate_backend(DocSt, texinfo) ->
	    % The text of the top section
	    % (includes the version in 'info')
            FrontR = if(info, [
                        % TODO: enable! (other info files look like this)
			%quotation(CopyrightR),
			VersionR])
	; FrontR = nop
	),
	% Put the summary in a section
	fmt_summary(VersionR, DocSt, SummaryR2),
	% Copyright section
	fmt_copyright(CopyrightR, DocSt, CopyrightR2),
	%
	doctree_simplify([FrontR, SummaryR2, CopyrightR2, IntroR, MainRest, EndR], DocR),
	% The main document section
	fmt_module_(ModuleType, SecProps00, DocR, DocSt, ModR).
fmt_module(DocSt, ModuleType,
	   Version, GlobalVers,
	   ModR) :-
	doc_message("Generating component documentation file", DocSt),
	% Contents inside the cartouche
	module_idx(ModuleType, DocSt, Ridx),
	get_authors(DocSt, AuthorRs),
	fmt_authors(AuthorRs, Ra),
	fmt_version(Version, GlobalVers, VerR),
	%
	get_doc(module, note, DocSt, CommentR),
	add_lines(CommentR, CommentR2),
	%
	Rcart = [Ra, VerR, CommentR2],
	%
	( ModuleType = part ->
	    Rcart2 = optional_cartouche(Rcart)
	; Rcart2 = Rcart
	),
	%
	doc_interface(DocSt, ModuleType, InterfaceR),
	fmt_other_info(DocSt, OtherR),
	%
	doctree_simplify([Ridx, Rcart2, InterfaceR, OtherR], DocR),
	%
	fmt_module_(ModuleType, [], DocR, DocSt, ModR).

fmt_module_(ModuleType, SecProps00, DocR, DocSt, ModR) :-
	get_doc(title, warning, DocSt, TitleR),
	title_for_module_type(ModuleType, TitleR, DocSt, TitleR2),
        ( doc_doing_mainmod(DocSt) ->
	    SectLabel = global_label(_),
	    %
	    doc_opt('-paper-opts'(StartPage, PaperType), DocSt),
	    SecProps = [paper_opts(StartPage, PaperType)|SecProps00]
	; % not main file
	  SecProps0 = [level(1)|SecProps00],
	  ( ModuleType = part ->
	      SecProps = [unnumbered|SecProps0],
	      doctree_to_rawtext(TitleR2, DocSt, RwTitle),
	      SectLabel = global_label("*** "||RwTitle)
	  ; SecProps = SecProps0,
	    SectLabel = global_label(_)
	  )
	),
	get_doc(pragma, ignore, DocSt, Pragmas),
	SecProps2 = [pragmas(Pragmas)|SecProps],
	%
	ModR0 = section_env([first_file_section|SecProps2], SectLabel, TitleR2, DocR),
	insert_printtoc(ModR0, DocSt, ModR).

fmt_introduction(ModuleType, OtherR, DocSt, IntroR, MainRest) :-
	doc_interface(DocSt, ModuleType, InterfaceR),
	get_doc(module, note, DocSt, CommentR),
	add_lines(CommentR, CommentR2),
	%
	IntroProps0 = [level(1),subfile('intro')],
	IntroR0 = [CommentR2, InterfaceR, IntroRest],
	( setting_value(html_layout, 'website_layout') ->
	    % TODO: generalize
	    % Do not emit a section for the introduction
	    IntroRest = [],
	    MainRest = [OtherR],
	    IntroR = IntroR0
	; % Emit a section for the introduction
          ( doc_no_components(DocSt) ->
	      IntroProps = [unnumbered|IntroProps0],
	      doc_modname(DocSt, NDName),
	      atom_codes(NDName, IntroTitle),
	      IntroRest = [],
	      MainRest = [OtherR]
	  ; IntroProps = IntroProps0,
	    IntroTitle = "Introduction",
	    IntroRest = [OtherR],
	    MainRest = []
	  ),
	  IntroR = section_env(
                     IntroProps,
		     global_label(_),
		     string_esc(IntroTitle),
		     IntroR0)
	).

fmt_summary(VersionR, DocSt, SummaryR2) :-
	get_mod_doc(summary, DocSt, SummaryR),
	( doctree_is_empty(SummaryR) ->
	    SummaryR2 = nop
	; % Add version to the summary (if not 'info')
	  ( doctree_is_empty(VersionR) ->
	      Rv3 = nop
	  ; docstate_backend(DocSt, texinfo) ->
	      Rv3 = [if(notinfo, VersionR)]
	  ; Rv3 = [VersionR]
	  ),
	  % The summary section
	  add_lines(SummaryR, SummaryR1),
	  ( summary_in_cover(DocSt) ->
	      Opts = [level(1)],
	      Label = local_label(_)
	  ; Opts = [unnumbered,level(1),subfile('summary')],
	    Label = global_label(_)
	  ),
	  SummaryR2 = section_env(Opts,
	                          Label,
				  string_esc("Summary"),
				  [SummaryR1, Rv3])
	).

fmt_copyright(CopyrightR, DocSt, CopyrightR2) :-
	( use_copyright_section(DocSt),
	  \+ doctree_is_empty(CopyrightR) ->
	    CopyrightR2 = section_env([level(1), subfile('copyright')],
	                              global_label(_),
				      string_esc("Copyright"),
				      CopyrightR)
	; CopyrightR2 = nop
	).

% Use a copyright section?
use_copyright_section(DocSt) :-
	docstate_backend(DocSt, html).
% Place the summary text in the cover
summary_in_cover(DocSt) :-
	docstate_backend(DocSt, html).

get_coversec_prop(GlobalVers, AuthorRs, CopyrightR, DocSt, CoverProp) :-
	( just_a_version(GlobalVers) ->
	    gen_version_note(GlobalVers, GlobalVersR),
	    gen_short_version_note(GlobalVers, GlobalVersShortR)
	; GlobalVersR = nop,
	  GlobalVersShortR = nop
	),
	get_mod_doc(subtitle, DocSt, SubtitleRs),
	CoverProp = coversec(SubtitleRs,
			     AuthorRs,
			     GlobalVersShortR,
			     GlobalVersR,
			     CopyrightR).

module_idx(ModuleType, DocSt, Ridx) :-
	( ModuleType \== part,
	  ( ModuleType = application ->
	      TI = apl
	  ; TI = lib
	  ),
	  docstate_has_index(TI, DocSt) ->
	    doc_modname(DocSt, NDName),
	    atom_codes(NDName, NDNameC),
	    % note: use a global_label(_) for anchor id since we point to the module itself
	    Ridx = idx_env(def, TI, global_label(_), string_esc(NDNameC), [])
	; Ridx = nop
	).

fmt_authors(AuthorRs, Ra) :-
	( AuthorRs = [] -> Ra = []
	; fmt_commas_period(AuthorRs, AuthorListR),
          Ra = [raw_nl,
	        bf(string_esc("Author(s):")),
		string_esc(" "),
		AuthorListR, p("")]
	).

title_for_module_type(ModuleType, TitleR, DocSt, TitleR2) :-
	( doctree_is_empty(TitleR) ->
	    doc_modname(DocSt, NDName),
	    atom_codes(NDName, NTitle),
	    ( doc_doing_mainmod(DocSt) ->
	        TitleR2 = [string_esc(NTitle), string_esc(" Reference Manual")]
	    ; ModuleType = application ->
	        TitleR2 = [string_esc(NTitle), string_esc(" (application)")]
	    ; ModuleType = part ->
		TitleR2 = string_esc(NTitle)
	    ; TitleR2 = [string_esc(NTitle), string_esc(" (library)")]
	    )
	; TitleR2 = TitleR
	).

% Adding including commands for components, the bibliography section,
% and sections for indices.
fmt_includes_and_biblio_and_indices(DocSt, EndR) :-
	all_component_specs(Components),
	gen_components_include(Components, CsR),
	%
	( doc_opt('-nobiblio', DocSt) ->
	    BiblioR = []
	; gen_biblio_section(DocSt, BiblioR)  
	),
	%
	all_indices(DocSt, Indices),
	gen_index_sections(Indices, DocSt, IndicesR),
	EndR = [CsR, BiblioR, IndicesR].

gen_components_include([],                      []).
gen_components_include([Component0|Components], [CR|CsR]) :-
	get_name(Component0, Base),
	CR = section_link(external(Base)),
	gen_components_include(Components, CsR).

gen_short_version_note(Version, R) :-
	version_string(Version, VersionStr),
	R = [string_esc("Version "), string_esc(VersionStr)].

gen_version_note(Version, R) :-
	version_string(Version, VersionStr),
	R = [sp("1"),
	     string_esc("This documentation corresponds to version "),
	     string_esc(VersionStr),
	     string_esc("."),
	     raw_nl].

fmt_version(Version, GlobalVers, [Rov1, Rov2]) :-
	gen_opt_version_field("Version:", GlobalVers, Rov1),
	( Version == GlobalVers -> Rov2 = []
	; gen_opt_version_field("Version of last change:", Version, Rov2)
	).

gen_opt_version_field(Text, Version, R) :-
	( just_a_version(Version) ->
	    version_string(Version, VersionStr),
	    R = [raw_nl,
	         bf(string_esc(Text)),
		 string_esc(" "),
		 string_esc(VersionStr),
		 raw_nl]
	; R = []
	).

% ---------------------------------------------------------------------------

:- pred doc_interface/3 # "Document the module interface.".

doc_interface(_, ModuleType, R) :-
	( ModuleType = application ; ModuleType = part ),
	!,
	R = [].
doc_interface(DocSt, ModuleType, R) :-
	doc_customdic_get(DocSt, modinfo, ModSt),
	ModSt = modinfo(M, Base),
	%
	doc_message("Generating library header...", DocSt),
	%
	% Exported predicates
	export_list(ModuleType, Base, DocSt, AllExports),
	eliminate_hidden(AllExports, Exports),
	% Multifiles
	findall(F/A, def_multifile(Base, F, A, _), RMultifiles),
	eliminate_hidden(RMultifiles, Multifiles),
	( ( Exports=[], Multifiles=[],
	    \+ ( ModuleType = include ; ModuleType = package )
	  ) ->
	    docstate_inputfile(DocSt, I),
	    warning_message(loc(I, 1, 1),
		"no exported predicates to be documented", [])
	; true
	),
	%
	% Imported modules
	findall(IFile, uses_file(Base, IFile), IFiles),
	% Other user files loaded
	findall(IUFile, adds(Base, IUFile), IUFiles),
	%
	% Source files whose contents should not be documented
	get_doc(nodoc, ignore, DocSt, NoDocS),
	doc_message("Not documenting: ~w", [NoDocS], DocSt),
	%
	% The ops (only "exported" if package or include)
	( ( ModuleType = include ; ModuleType = package ) ->
	    findall(op(P, Prec, PredNames),
		( clause_read(_, 1, op(P, Prec, PredNames), _, S, _, _),
		    no_path_file_name(S, FN),
		    basename(FN, BN),
		    \+ member(BN, NoDocS) ),
		Ops),
	    normalize_ops(Ops, SOps)
	; SOps=[]
	),
	%
	% The modes (only "exported" if package or include)
	( ( ModuleType = include ; ModuleType = package ) ->
	    findall(F/A, ( assertion_read(ModeP, M, _, modedef, _, _, S, _, _),
		    no_path_file_name(S, FN),
		    basename(FN, BN),
		    \+ member(BN, NoDocS),
		    functor(ModeP, F, A) ),
		CModes),
	    eliminate_duplicates(CModes, NModes)
	; NModes = []
	),
	%
	% Gather all decls to be documented. 
	% ??? Not a good idea???
	( ( ModuleType = include ; ModuleType = package ) ->
	    % document all having an explicit comment in the module:
	    findall(F/A, ( assertion_read(DeclP, M, _, decl, _, _, S, _, _),
		    no_path_file_name(S, FN),
		    basename(FN, BN),
		    \+ member(BN, NoDocS),
		    functor(DeclP, F, A) ),
		CDecls),
	    % also those having a new_declaration in the module
	    findall(NDP,
		( clause_read(Base, 1, new_declaration(NDP), _, S, _, _),
		    no_path_file_name(S, FN),
		    basename(FN, BN),
		    \+ member(BN, NoDocS)
		),
		NDDecls),
	    append(CDecls, NDDecls, PDupDecls),
	    % E.g., in case of being in both cases above
	    eliminate_duplicates(PDupDecls, NDecls)
	; NDecls=[]
        ),
	%
	% Internals  
	get_doc(doinclude, ignore, DocSt, Preds),
	filter_out_exports(Preds, Exports, Internals),
	%
	classify_exports(Exports, M, Base, CExports),
	classify_files(IFiles, UFiles, SysFiles, EngFiles, DocSt),
	%
	fmt_module_usage(DocSt, ModuleType, CExports, Multifiles,
	    UFiles, IUFiles, SysFiles, EngFiles, SOps, NDecls,
	    NModes, ModuleUsageR),
	%
	% new declarations
	fmt_predicates_kind(decl, "new declarations", NDecls, ModSt, DocSt, DeclsR),
	% any modes defined
	fmt_predicates_kind(modedef, "new modes", NModes, ModSt, DocSt, ModesR),
	% exported predicates, props, etc.
	fmt_predicates_kind(nodecl, "exports", Exports, ModSt, DocSt, ExportsR),
	% multifile predicates
	fmt_predicates_kind(nodecl, "multifiles", Multifiles, ModSt, DocSt, MultifilesR),
	% predicates for which it is explicitly requested (via a
	% @tt{:- doc(doinclude,<PredName>)} directive)
	fmt_predicates_kind(_IsDecl, "internals", Internals, ModSt, DocSt, InternalsR),
	R = [ModuleUsageR, DeclsR, ModesR, ExportsR, MultifilesR, InternalsR].

% ======================================================================

:- doc(subsection, "Bibliography Section").

:- use_module(library(make(make_rt)), [get_name/2]).

% The section where bibliography goes.
% TODO: indices are not generated here, but the section where they go
gen_biblio_section(_DocSt, BiblioR) :-
	Title = "References", 
	% TODO: use linktop or not?
	BiblioR = section_env([/*linktop,*/unnumbered,level(1),subfile('refs')],
                              global_label(_), 
                              string_esc(Title),
                              [
			        printbiblio
                              ]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Index Sections").

gen_index_sections([], _DocSt, []).
gen_index_sections([IdxName|Is], DocSt, [IndexR|IsR]) :-
	gen_index_section(IdxName, DocSt, IndexR),
	gen_index_sections(Is, DocSt, IsR).

% The sections where each index goes.
% TODO: This was said previously, but it is not longer true, fix?
%       "Each index goes in a separate file unless no components"
%       (then they go inline)
gen_index_section(IdxName, DocSt, IndexR) :-
	get_idxsub(IdxName, SubName),
	typeindex(IdxName, IndexId, _, ITitle, IComment),
	docstate_currmod(DocSt, Name),
	add_lines(IComment, IComment2),
	IndexR = section_env([linktop,unnumbered,level(1),subfile(SubName)],
                             global_label(_), 
                             string_esc(ITitle), 
                             [
			       IComment2,
			       printindex(Name, IndexId)
                             ]).

% ======================================================================

:- doc(fmt_module_usage(DocSt,ModuleType,Exports,Mults,
   UMods,IUMods,SMods,EMOds,Ops,NDecls,NModes,R), "This
   predicate defines the format of the usage info for the
   module. @var{Exports} contains the predicates
   exported by the module (taken from the @pred{module/2}
   declaration). @var{UMods} contains the user modules imported by the
   module. @var{SMods} contains the system modules imported by the
   module. @var{EMods} contains the internal (engine) modules imported
   by the module. @var{Ops} contains any exported operator
   definitions. @var{NDecls} contains any exported new
   declarations. @var{NModes} contains any new mode
   definitions.").

:- pred fmt_module_usage(DocSt, ModuleType, Exports, Mults, UMods, IUMods,
	    SysMods, EngMods, Ops, NDecls, NModes, R)
	: ( docstate(DocSt), modtype(ModuleType), list(Exports, predname),
	    list(Exports, predname), list(UMods, atm), list(IUMods, atm),
	    list(SysMods, atm),
	    list(EngMods, atm), list(Ops), list(NDecls, atm), list(NModes, atm),
	    doctree(R) )
# "The module header info is documented as the first section of the
   chapter.".
fmt_module_usage(DocSt, ModuleType, CExports, Mults,
	    UMods, IUMods, SysMods, EngMods, Ops, NDecls,
	    NModes, R) :-
	get_doc(usage, ignore, DocSt, UsageR0),
	( is_nonempty_doctree(UsageR0) ->
	    % Usage comment to override automatic one
	    UsageR = UsageR0
	; doc_modname(DocSt, NDName),
	  filetype_usage_command(ModuleType, Cmd),
	  % TODO: make sure that module spec is correct! (it is not now)
	  ( ModuleType = package ->
	      format_to_string(":- ~w(~w).", [Cmd, NDName], UseDeclR0),
	      format_to_string(":- module(...,...,[~w]).", [NDName], UseDeclR1),
	      % TODO: use linebreak? or p("")?
	      UsageR = [tt(string_esc(UseDeclR0)), raw_nl, raw_nl, string_esc("or"), raw_nl, raw_nl, tt(string_esc(UseDeclR1))]
	  ; format_to_string(":- ~w(library(~w)).", [Cmd, NDName], UseDeclR0),
            UsageR = tt(string_esc(UseDeclR0))
	  )
	),
	gen_item(bf, string_esc("Library usage"), UsageR, Pa1),
	%
	( CExports = [] ->
	    Pa2 = nop
	; Filters = [("PREDICATE", string_esc("Predicates")),
	             ("FUNCTION", string_esc("Functions")),
		     ("PROPERTY", string_esc("Properties")),
		     ("FUNCTION", string_esc("Functions")),
		     ("REGTYPE", string_esc("Regular Types")),
		     %% (enable to document modes)
		     % ("MODE", string_esc("Modes")),
		     ("ENTRY POINT", string_esc("Entry points"))],
	  gen_classified_export_cases(Filters, CExports, E1),
	  gen_cases(em, string_esc("Multifiles"), Mults, E2),

	  gen_item(bf, string_esc("Exports"), itemize_minus([E1, E2]), Pa2)
	),
	gen_cases(bf, string_esc("New operators defined"), Ops, Ro),
	gen_cases(bf, string_esc("New modes defined"), NModes, Rm),
	gen_cases(bf, string_esc("New declarations defined"), NDecls, Rd),
	( UMods = [], SysMods = [], EngMods = [] ->
	    Pa3 = nop
	; gen_cases(em, string_esc("Application modules"), UMods, L1),
	  gen_cases(em, [string_esc("Files of module "), tt(string_esc("user"))], IUMods, L2),
	  gen_cases(em, string_esc("System library modules"), SysMods, L3),
	  gen_cases(em, string_esc("Internal (engine) modules"), EngMods, L4),
	  gen_item(bf, string_esc("Other modules used"), itemize_minus([L1, L2, L3, L4]), Pa3)
	),
	% TODO: this section_env contains more things!
	R = section_env(
              [with_parent,level(2)],
              local_label(_),
              string_esc("Usage and interface"),
	      cartouche(itemize_bullet([Pa1, Pa2, Ro, Rm, Rd, Pa3]))
            ).

filetype_usage_command(module,  use_module).
filetype_usage_command(user,    ensure_loaded).
filetype_usage_command(include, include).
filetype_usage_command(package, use_package).

gen_classified_export_cases([], _CExports, []).
gen_classified_export_cases([(Type, LabelR)|Xs], CExports, [R|Rs]) :-
	filter_export_by_type(CExports, Type, CExports2),
	gen_cases(em, LabelR, CExports2, R),
	gen_classified_export_cases(Xs, CExports, Rs).

gen_item(bf, TextR, B, R) :-
	R = [item(""), bf([TextR, string_esc(":")]), linebreak, B].
gen_item(em, TextR, B, R) :-
	R = [item(""), em([TextR, string_esc(":")]), linebreak, B].

gen_cases(Style, LabelR, Xs, R) :-
	( Xs = [] ->
	    R = nop
	; fmt_terms(Xs, code, Xs1), % TODO: 'code' could be made more specific (e.g. lib, op, ...)
	  fmt_commas_period(Xs1, Xs2),
	  gen_item(Style, LabelR, [Xs2, raw_nl], R)
	).

% ----------------------------------------------------------------------

filter_export_by_type([],                        _Type,   []).
filter_export_by_type([export(F/A, Type)|CExps], TypeStr, [F/A|FExps]) :-
	assrt_type_text(Type, TypeStr, _, _),
	!,
	filter_export_by_type(CExps, TypeStr, FExps).
filter_export_by_type([_|CExps], TypeStr, FExps) :-
	!,
	filter_export_by_type(CExps, TypeStr, FExps).

%% %% Version when also documenting imports:
%% handle_export_cases([],[],_DocSt,_OS) :- !.
%% handle_export_cases([],Mods,_DocSt,OS) :- 
%% 	Mods \== [], !,
%% 	format(OS,"~n@item @strong{Exports:} 
%%                     see exports of imported modules.~n~n",[]).
%% handle_export_cases(Exports,[],DocSt,OS) :- 
%% 	Exports \== [], !,
%% 	format(OS,"~n@item @strong{Exports:}~n~n",[]),
%% 	fmt_terms_commas_period(Exports,global,DocSt,OS).
%% handle_export_cases(Exports,Mods,DocSt,OS) :-
%% 	Exports \== [], Mods \== [], !,
%% 	format(OS,"~n@item @strong{Exports} (see also exports of imported 
%%                    modules):~n~n",[]),
%% 	fmt_terms_commas_period(Exports,global,DocSt,OS).

%%%% Alternative version (using a table)
%% 	format(OS,"@sp 1~n@table @strong~n",[]),
%% 	format(OS,"~n@item @strong{Library usage:}~n",[]),
%% 	format(OS,"@code{:- use_module(library(~w))}~n",[Name]),
%% 	format(OS,"~n@item @strong{Exported predicates:}~n",[]),
%% 	fmt_terms_commas_period(Exports,no_ref,OS),
%% 	( UMods = [] -> true
%%         ; 
%% 	  format(OS,"~n@item @strong{Other modules used:}~n",[]),
%% 	  fmt_terms_commas_period(UMods,no_ref,OS) ),
%% 	format(OS, "@end table~n",[]).

%% fmt_terms_commas_period(Cs, Type, Rs) :-
%% 	fmt_terms(Cs, Type, Rs0),
%% 	fmt_commas_period(Rs0, Rs).

% ----------------------------------------------------------------------

:- doc(subsection, "Appendix, Acknowledges, Changelog and Bugs").

fmt_other_info(DocSt, OtherR) :-
	fmt_appendix(DocSt, AppendixR2),
	fmt_acknowledges(DocSt, AckR2),
	fmt_bugs(DocSt, BugsR),
	fmt_changes(DocSt, ChangesR),
	OtherR = [AppendixR2, AckR2, BugsR, ChangesR].

fmt_appendix(DocSt, AppendixR2) :-
	get_doc(appendix, ignore, DocSt, AppendixR),
	add_lines(AppendixR, AppendixR1),
	optional_section('appdx', "Other information", AppendixR1, DocSt, AppendixR2).

fmt_acknowledges(DocSt, AckR2) :-
	get_doc(ack, ignore, DocSt, AckR),
	add_lines(AckR, AckR1),
	optional_section('ack', "Acknowledgments", AckR1, DocSt, AckR2).

fmt_bugs(DocSt, BugsR) :-
	( doc_opt('-nobugs', DocSt) ->
	    BugRs=[]
	; get_doc(bug, ignore, DocSt, BugRs)
	),
	gen_bugs(BugRs, Bugs2),
	optional_section('bugs', "Known bugs and planned improvements", Bugs2, DocSt, BugsR).

gen_bugs([], nop) :- !.
gen_bugs(Xs, R) :-
	gen_changes_(Xs, Items),
	R = itemize_bullet(Items).

fmt_changes(DocSt, ChangesR) :-
	( doc_opt('-nochangelog', DocSt) ->
	    Changes = []
	; ( doc_opt('-nopatches', DocSt) ->
	      VPatch = 0
	  ; true
	  ),
	  ( setof(Change,
	      VPatch^change_field(VPatch, DocSt, Change),
	      RChanges) ->
	        reverse(RChanges, Changes)
	  ; Changes = []
	  )
	),
	gen_changes(Changes, Changes2),
	optional_section('changes', "Version/Change Log", Changes2, DocSt, ChangesR).

change_field(VPatch, DocSt, change(Version, RC)) :-
	version_patch(V, VPatch),
	get_doc_field(V, C, Loc),
	( V = version(Ver, Date) ->
	    Version = version(Ver, Date, [])
	; Version = V
	),
	parse_docstring_loc(DocSt, Loc, C, RC).

gen_changes([], nop) :- !.
gen_changes(Xs, R) :-
	gen_changes_(Xs, Items),
	R = description_env(Items).

gen_changes_([],                                []).
gen_changes_([change(Version, ChangeTextR)|Changes], [C|Cs]) :- !,
	version_string(Version, VersionStr),
	C = [item(bf(string_esc("Version "||VersionStr))), ChangeTextR],
	gen_changes_(Changes, Cs).
gen_changes_([BugR|BugRs], [C|Cs]) :-
	C = [item(""), BugR],
	gen_changes_(BugRs, Cs).

% ---------------------------------------------------------------------------

% TODO: Generalize and reuse this code for other sections that can be omitted
%   if empty.
optional_section(Sub, Title, BodyR, DocSt, SectR) :-
	( doctree_is_empty(BodyR) ->
	    SectR = nop
	; optional_props(Sub, DocSt, SecProps, SectLabel),
	  % display(user_error, op(Sub, SecProps, SectLabel)), nl(user_error),
	  SectR = section_env(
            SecProps,
	    SectLabel,
	    string_esc(Title),
	    BodyR
          )
	).

optional_props(Sub, DocSt, SecProps, SectLabel) :-
	( doc_doing_mainmod(DocSt) ->
	    ( doc_no_components(DocSt) ->
	        %% If level is 0 do not uniquify
	        SecProps = [level(0),subfile(Sub)],
		SectLabel = global_label(_)
	    ; SecProps = [level(2)],
	      SectLabel = local_label(_)
	    )
	; SecProps = [with_parent,level(2)],
	  SectLabel = local_label(_)
	).

% Add optional line break
% TODO: Wrong if we add lines and there is no following text; it could
%       be easier if we work with paragraphs (or simplify linebreaks
%       later)
add_lines(R, R) :- doctree_is_empty(R), !.
add_lines(R0, R) :- R = [raw_nl, R0, linebreak].

% Remove the text detailing the author contribution
% TODO: Not documented!
remove_author_details(Text, Name) :-
	append(Name0, "("||_, Text),
	reverse(Name0, Name1),
	remove_leading_blanks(Name1, Name2),
	reverse(Name2, Name).

% TODO: useful?
remove_leading_blanks([X|Xs], Ys) :- is_blank(X), !,
 	remove_leading_blanks(Xs, Ys).
remove_leading_blanks(Xs, Xs).
 
is_blank(0' ).
is_blank(0'\t).
is_blank(0'\n).

:- regtype change/1.
change(change(_Version, _Change)).
change(_).

% ======================================================================

fmt_terms([], _Type, []) :- !.
fmt_terms([C|Cs], Type, [R|Rs]) :-
	fmt_term(C, Type, R),
	fmt_terms(Cs, Type, Rs).

% Format a term of a given 'type' (if necessary, add the commands to
% include in its corresponding index too)
fmt_term(op(Prec, Assoc, EFunctor), Type, R) :- !,
	op_arity(Assoc, Arity),
	C2 = EFunctor/Arity,
	fmt_code_spec(C2, ER),
	( Type = no_ref ->
	    RR = []
	; RR = idx_env(use, Type, localnum_label(_), ER, ER)
	),
	format_to_string(" [~w,~w]", [Prec, Assoc], PrecS),
	R = [RR, string_esc(PrecS)].
fmt_term(C, Type, R) :- !,
	fmt_code_spec(C, ER),
	( Type = no_ref ->
	    RR = []
	; RR = idx_env(use, Type, localnum_label(_), ER, ER)
	),
	R = RR.

op_arity(fy,  1).
op_arity(yf,  1).
op_arity(fx,  1).
op_arity(xf,  1).
op_arity(xfx, 2).
op_arity(xfy, 2).
op_arity(yfx, 2).

fmt_code_spec(C, S1) :-
	( C = F/A ->
	    %% This is to avoid parenthesis around operators...
	    format_to_string("~w/~w", [F, A], S)
	; format_to_string("~w", [C], S)
	),
	S1 = string_esc(S).

%% ---------------------------------------------------------------------------
:- pred export_list/4 # "Builds the list of exported
   predicates. Handles the special case of @tt{user} files.".
% TODO: also 'include' and 'package'?

export_list(module, Base, _DocSt, AllExports) :-
	!,
	findall(F/A, exports(Base, F, A, _, _), AllExports).
%% We may need to add here the case of predicates which are not defined 
%% but for which there is an assertion?
export_list(_ModuleType, Base, DocSt, AllExports) :-
	findall(F/A, defines(Base, F, A, _, _), DupAllExports),
	eliminate_duplicates(DupAllExports, AllExports),
	doc_message("Documenting all defined predicates: ~w",
	    [AllExports], DocSt).

%% ---------------------------------------------------------------------------
:- pred eliminate_hidden/2 # "Eliminates from the export list those
   predicates affected by a comment with @tt{hide} in the second
   argument.".

eliminate_hidden([],           []).
eliminate_hidden([Pred|Preds], EPreds) :-
	get_doc_field(hide, Pred, _),
	!,
	eliminate_hidden(Preds, EPreds).
eliminate_hidden([Pred|Preds], EPreds) :-
	get_doc_field(hide, PredList, _),
	list(PredList),
	member(Pred, PredList),
	!,
	eliminate_hidden(Preds, EPreds).
eliminate_hidden([Pred|Preds], [Pred|EPreds]) :-
	eliminate_hidden(Preds, EPreds).

%% ---------------------------------------------------------------------------
:- pred normalize_ops/2 # "Flattens out the cases where several ops
   are defined in the same declaration.".

normalize_ops([],       []).
normalize_ops([Op|Ops], [Op|NOps]) :-
	Op = op(_, _, L),
	atom(L),
	!,
	normalize_ops(Ops, NOps).
normalize_ops([Op|Ops], TNOps) :-
	Op = op(Prec, Style, LPred),
	normalize_ops_list(LPred, Prec, Style, TNOps, TNOpsE),
	!,
	normalize_ops(Ops, TNOpsE).

normalize_ops_list([],           _Prec, _Style, NOpsE,
	    NOpsE).
normalize_ops_list([Pred|Preds], Prec,  Style,  [op(Prec, Style, Pred)|NOps],
	    NOpsE) :-
	normalize_ops_list(Preds, Prec, Style, NOps, NOpsE).

%% ---------------------------------------------------------------------------
:- pred classify_exports/4 # "Classifies exported predicates as
   predicates, declarations, etc. according to the declared type in
   associated assertions. Also captures the special case of regular
   types (by detecting the corresponding property in the comp part).".

classify_exports([], _M, _Base, []).
% if local, look in local assertions
classify_exports([F/A|Exps], M, Base, AllExps) :-
	functor(P, F, A),
	check_types_in_assertions(P, F, A, M, Exports),
	Exports \== [],
	!,
	append(Exports, CExps, AllExps),
	classify_exports(Exps, M, Base, CExps).
% if imported, look in assertions from that module
classify_exports([F/A|Exps], M, Base, AllExps) :-
	functor(P, F, A),
	imports_pred(Base, UM, F, A, _, _, _),
	check_types_in_assertions(P, F, A, UM, Exports),
	!,
	append(Exports, CExps, AllExps),
	classify_exports(Exps, M, Base, CExps).
% else, assume pred.
classify_exports([F/A|Exps], M, Base, [export(F/A, pred)|CExps]) :-
	classify_exports(Exps, M, Base, CExps).

%% Includes special case for regular types.
check_types_in_assertions(P, F, A, M, Exports) :-
	findall(export(F/A, PType),
	    ( assertion_read(P, M, _Status, Type, NAss, _Dict, _S, _LB, _LE),
		predfunctor(Type),
		patch_special_prop(Type, NAss, PType) ),
	    DExports),
	eliminate_duplicates(DExports, Exports).

patch_special_prop(Type, NAss, NewType) :-
	Type == prop,
	assertion_body(_, _, _, _, GP, _, NAss),
	special_prop(IdentifyingProp, NewType),
	member(IdentifyingProp, GP),
	!.
patch_special_prop(Type, _NAss, Type).

%% ---------------------------------------------------------------------------
:- pred special_prop(CProp, Type) # "If a property definition has
   @var{CProp} in its comp (+) field, then it is a special property of
   type @var{Type}.".

%% Native properties should be added here also? Perhaps dynamically?

special_prop(regtype(_), regtype).

%% ---------------------------------------------------------------------------
:- pred classify_files/5 # "Classifies file references, such as
   @tt{library(aggregates)}, into separate lists according to whether
   they are System, Engine, User, etc.".

classify_files(IFiles, UFiles, SysFiles, EngFiles, DocSt) :-
        doc_opt('-lib-opts'(LibPaths, SysLibPaths, _PathAliasF), DocSt),
	classify_files_(IFiles, LibPaths, SysLibPaths, UFiles, SysFiles, EngFiles, DocSt).

classify_files_([],           _LibPaths, _SysLibPaths, [],     [],     [],
	    _).
classify_files_([File|Files], LibPaths,  SysLibPaths,  UFiles, SFiles, OEFiles,
	    DocSt) :-
	File =.. [engine, EFile],
	!,
	( doc_opt('-noengmods', DocSt) ->
	    OEFiles = EFiles
	; OEFiles = [EFile|EFiles]
	),
	classify_files_(Files, LibPaths, SysLibPaths, UFiles, SFiles, EFiles, DocSt).
classify_files_([RFile|Files], LibPaths, SysLibPaths, UFiles, OSFiles, EFiles, DocSt) :-
	base_name(RFile, Base), % Daniel says, this is the way to do it
	member(Path, SysLibPaths),
	atom_concat(Path, LongFileName, Base),
	atom_concat('/',  FullFile,     LongFileName), % Eliminate leading /
	!,
	( doc_opt('-nosysmods', DocSt) ->
	    OSFiles = NSM
	; OSFiles = [FullFile|NSM]
	),
	classify_files_(Files, LibPaths, SysLibPaths, UFiles, NSM, EFiles, DocSt).
classify_files_([File|Files], LibPaths, SysLibPaths, [File|UFiles], SysFiles,
	    EFiles, DocSt) :-
	classify_files_(Files, LibPaths, SysLibPaths, UFiles, SysFiles, EFiles, DocSt).

%% ---------------------------------------------------------------------------

:- pred fmt_predicates_kind/6
# "Generates documentation for predicates or declarations of a given kind.".

fmt_predicates_kind(Kind, Desc, Items, ModSt, DocSt, R) :-
	doc_message("Documenting "||Desc, DocSt),
	( Items = [] ->
	    R = []
	; doc_predicates(Items, Kind, ModSt, DocSt, ItemsR),
	  Title = "Documentation on "||Desc,
	  R = section_env([with_parent,level(2)], local_label(_), string_esc(Title), ItemsR)
	).

%% ---------------------------------------------------------------------------
:- pred filter_out_exports/3 # "Eliminates the predicates already
   documented as exports so that they are not documented twice.".

filter_out_exports([],          _Exports, []).
filter_out_exports([F/A|Preds], Exports,  FPreds) :-
	member(F/A, Exports),
	!,
	filter_out_exports(Preds, Exports, FPreds).
filter_out_exports([PredList|Preds], Exports, FPreds) :-
	%% doc/2 argument is list
	list(PredList),
	!,
	filter_out_exports(PredList, Exports, FilteredPreds),
	filter_out_exports(Preds,    Exports, OtherFilteredPreds),
	append(FilteredPreds, OtherFilteredPreds, FPreds).
filter_out_exports([Pred|Preds], Exports, [Pred|FPreds]) :-
	filter_out_exports(Preds, Exports, FPreds).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Document Predicates").

:- pred doc_predicates/5
# "Generates documentation for a list of predicates.

      One issue here, given that there may be reexports, is which
      assertions and code to use in the documentation. The best thing
      seems to be to use the assertions that are either in the file
      being documented or, if none exist, in the closest file in the
      reexport chain. This is symmetric with the fact that local code
      takes precedence over imported code.

      Thus, we treat the assertions in the current module first.
      Otherwise, we follow import chain.  ".

doc_predicates([],     _,    _ModSt, _DocSt, []).
doc_predicates([P|Ps], IsDecl, ModSt, DocSt, [R|Rs]) :-
	doc_predicate(P, IsDecl, ModSt, DocSt, R),
	doc_predicates(Ps, IsDecl, ModSt, DocSt, Rs).

%% General case:
doc_predicate(F/A, IsDecl, ModSt, DocSt, R) :-
	ModSt = modinfo(M, Base),
	%
	doc_message("Generating documentation for ~w:~w/~w", [M, F, A], DocSt),
	functor(P, F, A),
	predicate_usages(P, IsDecl, M, Usages, N, Multiple),
	predicate_level_comment(F/A, DocSt, CommentR, CommentHead),
	other_assertions(P, IsDecl, M, OtherAssrt, ON),
	( ( IsDecl == decl
	  ; IsDecl == modedef
	  ) ->
	    PType = IsDecl
	; look_for_pred_type(Usages, F/A, PType)
	),
	% Check that there are assertions, get assertion type
	( ( Usages \== []
	  ; OtherAssrt \== []
	  ; is_nonempty_doctree(CommentR)
	  ) ->
	    %% If there are any assertions, then succeed and thus 
	    %% definitely document with them.
	    NCommentR = CommentR
	; ( ( \+ imports_pred(Base, _, F, A, _, _, _)
	    ; defines(Base, F, A, _, _)
	    ) -> %% No assertions, and predicate is not imported: too bad
	      NCommentR = string_esc("No further documentation available for this predicate."),
	      get_first_loc_for_pred(F, A, Loc),
	      warning_message(Loc,
	        "no assertions or comments found for ~w ~w", [PType, F/A])
	  ; %% Else, probably imported (otherwise, fail globally)
	    fail
	  )
	),
	!,
	assrt_type_text(PType, PText, _, _),
	%% In case of explicit arguments, CP should also be included...
	( member(TmpAssrt, OtherAssrt),
	    TmpAssrt = assertion_read(_, _, _, _, TmpNAss, _, _, _, _),
	    assertion_body(_, _, _, _, TGP, _, TmpNAss),
	    member(iso(_), TGP)
	->  Standard = iso
	; Standard = non_iso
	),
	fmt_head_descriptor(CommentHead, PType, Standard, HeadR),
	%% Trying to catch props that are just declared with no comment:
	( doctree_is_empty(NCommentR),
	    Usages = [assertion_read(_, _, _, _, NAss, _, _, _, _)], %% N=1,
	    assertion_body(_, _, _, _, _, [], NAss), %% I.e., no comment
	    ( PType=prop, PropText= "property"
	    ; PType=regtype, PropText= "regular type"
	    )
	-> atom_codes(F, FS),
	    number_codes(A, AS),
	    list_concat(["A ", PropText,
		    ", defined as follows:@includedef{",
		    FS, "/", AS,
		    "}\n"], TNComment), %% Added \n
	    get_first_loc_for_pred(F, A, Loc),
	    note_message(Loc,
		"no comment text for ~s ~w, including definition", [PropText,
		    F/A]),
	    parse_docstring_loc(DocSt, Loc, TNComment, NNCommentR0)
	; NNCommentR0 = NCommentR
	),	
	( (CommentHead = _/_ ; doctree_is_empty(NNCommentR0)) ->
	    NNCommentR1 = NNCommentR0
	; NNCommentR1 = [p(""), NNCommentR0]
	),
	doc_native_declarations(F/A, M, Base, DocSt, NativeR),
	doc_other_assertions(OtherAssrt, ON, N, F/A, PType, DocSt, OtherAssrtR),
	doc_usages(Usages, 1, Multiple, F/A, PType, DocSt, UsagesR),
	( doctree_is_empty(NativeR),
	  doctree_is_empty(OtherAssrtR) ->
	    PredR = [UsagesR]
	; PredR = [linebreak, NativeR, OtherAssrtR, UsagesR]
	),
	R = [defpred(local_label(_), PType, PText, F/A, [
               HeadR, NNCommentR1, PredR
               ]),
             sp("1"), raw_nl].
doc_predicate(F/A, IsDecl, ModSt, DocSt, R) :-
	ModSt = modinfo(M, Base),
	imports_pred(Base, UFile, F, A, _DefType, _Meta, _EndFile),
	base_name(UFile, UBase),
	defines_module(UBase, UM),
	M \== UM, %% To handle engine preds: they appear as imported 
	%% in the file in which they are defined!
	!,
	( ( get_doc_field(doinclude, F/A, _)
	  ; ( get_doc_field(doinclude, PredList, _),
	      list(PredList),
	      member(F/A, PredList)
	    )
	  ) ->
	    doc_message(
		"following reexport chain for ~w to ~w", [F/A, UM], DocSt),
	    doc_predicate(F/A, IsDecl, modinfo(UM, UBase), DocSt, R)
	;
	    doc_message("~w reexported from ~w (not documented)", [F/A, UM], DocSt),
	    Type = udreexp,
	    assrt_type_text(Type, PText, _, _),
	    atom_codes(UM, UMS),
	    list_concat(["\n\Imported from @lib{", UMS,
		    "} (see the corresponding documentation for details)."],
		Text),
	    parse_docstring(DocSt, Text, RText),
	    add_lines(RText, RText1),
	    R = [defpred(local_label(_), Type, PText, F/A, [RText1]), sp("1"), raw_nl]
	).
doc_predicate(P, _, _ModSt, _DocSt, R) :-
	R = [],
	error_message(_, "could not document predicate ~w", [P]).

%% ---------------------------------------------------------------------------
%% Abstracted out parts of doc_predicate:

%% Get the assertions that describe usages (predfunctor type):
%% (do not get decl or modedef assrts; if documenting decl or modedef, 
%% then get only decl or modedef  assrts)
predicate_usages(P, IsDecl, M, Usages, N, Multiple) :-
	findall(assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
	    ( assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
		( nonvar(IsDecl)
		-> ( (IsDecl = decl ; IsDecl = modedef)
		    -> Type = IsDecl
		    ; ((\+ Type = decl), (\+ Type = modedef)) )
		; true ),
		predfunctor(Type),
		bind_dict_varnames(Dict)
	    ),
	    Usages),
	length(Usages, N), (N>1 -> Multiple=1; Multiple=0).

%% Get any comment declarations, compute CommentHead:
predicate_level_comment(F/A, DocSt, CommentR, CommentHead) :-
	functor(CP, F, A),
	( get_doc(pred(F/A), dofail, DocSt, CommentR),
	    CommentHead = F/A
	; get_doc(pred(CP), dofail, DocSt, CommentR),
	    CommentHead = CP
	; CommentHead = F/A, empty_doctree(CommentR)
	).


%% Get any other assertions:
%% (except for decls)
other_assertions(_P, IsDecl, _M, [], 0) :-
	IsDecl == decl,
	!.
other_assertions(P, _IsDecl, M, OtherAssrt, ON) :-
	findall(assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
	    ( assertion_read(P, M, Status, Type, NAss, Dict, S, LB, LE),
		\+ (predfunctor(Type)),
		bind_dict_varnames(Dict) ),
	    OtherAssrt),
	length(OtherAssrt, ON).

%% ---------------------------------------------------------------------------
:- pred look_for_pred_type(L, P, T) ::
	(list(L), predname(P), predfunctor_ext(T)) #
"@var{T} is the type of the predicate described by the assertions
   in @var{L} for predicate @var{P}.".

:- regtype predfunctor_ext/1.
predfunctor_ext(_).

%% If no explicit type found (e.g., only basic assertions) then assume pred
%% (unless explicitly declared as a new_declaration)
look_for_pred_type([], _, Type) :-
	nonvar(Type),
	!.
look_for_pred_type([], F/A, Type) :-
	var(Type),
	clause_read(_, 1, new_declaration(F/A), _, _, _, _),
	!,
	Type = decl.
look_for_pred_type([], _, Type) :-
	var(Type),
	!,
	Type = pred.
look_for_pred_type(
	    [assertion_read(_P, _M, _S, RType, NAss, _Dict, S, LB, LE)|R], _,
	    Type) :-
	patch_special_prop(RType, NAss, AType), %% Special case for regtypes
	handle_pred_type(AType, R, Type, loc(S, LB, LE)).

handle_pred_type(AType, R, Type, _Loc) :-
	var(Type),
	(predfunctor(AType) ; special_prop(_, AType)),
	!,
	%% We assume that this is the type.
	Type = AType,
	look_for_pred_type(R, _, Type).
handle_pred_type(AType, R, Type, Loc) :-
	nonvar(Type),
	predfunctor(AType),
	!,
	%% Must be identical to previously found type.
	( Type == AType -> true
	; warning_message(Loc,
	    "incompatible assertion types ~w and ~w", [Type, AType]),
	  fail
	),
	look_for_pred_type(R, _, Type).
handle_pred_type(_AType, R, Type, _Loc) :-
	%% Else, we continue looking.
	look_for_pred_type(R, _, Type).

%% ---------------------------------------------------------------------------
:- pred doc_native_declarations/5 # "Generates documentation for the
   native declarations, such as @decl{dynamic/1}, @decl{multifile/1},
   etc. Implicit is a special case.".

doc_native_declarations(F/A, _M, Base, _DocSt, NativeR) :-
	( defines(Base, F, A, Type, Meta) ->
	    fmt_native_declaration(normal(Type, Meta), DefR)
	; DefR = []
	),
	( def_multifile(Base, F, A, Mode) -> % (static, dynamic, concurrent)
	    fmt_native_declaration(multifile(Mode), MultiR)
	; MultiR = []
	),
	NativeR = [DefR, MultiR].

fmt_native_declaration(normal(Type, Meta), R) :- !,
	fmt_type_info(Type, TypeR),
	fmt_meta_info(Meta, MetaR),
	R = [TypeR, MetaR].
fmt_native_declaration(multifile(Type), R) :- !,
	MultiR = [raw_nl, string_esc("The predicate is "), em(string_esc("multifile")), string_esc("."),
	          linebreak],
	fmt_type_info(Type, TypeR),
        R = [MultiR, TypeR].
fmt_native_declaration(Mode, R) :-
	fmt_type_info(Mode, R).

%% Static and implicit not interesting to document.
fmt_type_info(static, R) :- !, R = [].
fmt_type_info(implicit, R) :- !, R = [].
fmt_type_info(Type, R) :-
	atom_codes(Type, TypeS),
	R = [raw_nl,
             string_esc("The predicate is of type "),
             em(string_esc(TypeS)),
	     string_esc("."),
	     linebreak].

fmt_meta_info(0, R) :- !, R = [].
fmt_meta_info(Meta, R) :-
	format_to_string("~w", [Meta], MetaS),
	R = [raw_nl,
	     em(string_esc("Meta-predicate")),
	     string_esc(" with arguments: "),
	     tt(string_esc(MetaS)),
	     string_esc("."),
	     linebreak].

%% ---------------------------------------------------------------------------
:- pred doc_other_assertions/7 # "Generates documentation for assertions 
   other than @tt{pred} assertions.".

doc_other_assertions(OtherAssrt,  ON,  N,  P,  Type, DocSt, R) :-
	( ON = 0 ->
	    R = []
	; ON > 0, N > 0 ->
	    gen_other_assrt_header(R0),
	    doc_other_assrts(OtherAssrt, P, Type, DocSt, R1),
	    R = [R0, R1]
	; ON > 0, N = 0 ->
	    doc_other_assrts(OtherAssrt, P, Type, DocSt, R)
	).

gen_other_assrt_header(R) :-
	R = [raw_nl, bf(string_esc("General properties:")), string_esc(" ")].

doc_other_assrts([], _P, _Type, _DocSt, []) :- !.
doc_other_assrts([Assrt|Assrts], _P, Type, DocSt, [AssrtR|AssrtsR]) :-
	doc_usage(Assrt, _, -1, Type, DocSt, AssrtR),	
	doc_other_assrts(Assrts, _P, Type, DocSt, AssrtsR).

%% ---------------------------------------------------------------------------
:- pred doc_usages/7 # "Generates documentation for each ``usage'' of
   a predicate (as declared in a @tt{pred} assertion).".

doc_usages([], _N, _M, _P, _Type, _DocSt, []) :- !.
doc_usages([Usage|Usages], N, Multiple, _P, Type, DocSt, [UsageR|UsagesR]) :-
	doc_usage(Usage, N, Multiple, Type, DocSt, UsageR),
	N1 is N+1,
	doc_usages(Usages, N1, Multiple, _P, Type, DocSt, UsagesR).

%% If no info, then don't document!
doc_usage(Assrt, _N, _Multiple, _Type, _DocSt, UsageR) :-
	Assrt = assertion_read(CP, _M, _Status, _Type, NAss, _, _, _, _),
	assertion_body(_, [], [], [], [], [], NAss),
	CP =.. [_|Args],
	allvars(Args),
	!,
	UsageR = [].
doc_usage(Assrt, N, Multiple, Type, DocSt, UsageR) :-
	Assrt = assertion_read(_P, _M, Status, AType, NAss, _, S, LB, LE),
	Loc = loc(S, LB, LE),
	assertion_body(P, DP, CP, AP, GP, CO, NAss),
	fix_var_arg_names(P, Loc, NP),
	( member(iso(_), GP), Multiple \== -1 -> %% Done differently for gen props
	    Standard=iso
	; Standard=non_iso
	),
	( doc_opt('-noisoline', DocSt),
	  select(iso(_), GP, NNGP) ->
	    true
	; NNGP = GP
	),
	( (\+ doc_opt('-regtypeprops', DocSt)),
	  select(regtype(_), NNGP, NGP)	->
	    true
	; NNGP = NGP
	),
	%
	( CO=[], DP=[], CP=[], AP=[], NGP=[] ->
	    UsageR = [] % No info
	; gen_usage_header(N, Multiple, HeaderR),
	  % Documenting a general property or empty usage
	  fmt_head_descriptor(NP, Type, Standard, HeadR),
	  %
	  doc_description(CO, Loc, NP, DocSt, DescR),
	  %% Cond used to see whether calls and comp props are conditional
	  (CP = [] -> Cond=empty ; Cond = full),
	  doc_site(compat, Loc, Cond, DP,  NP, Type,  Status, DocSt, DPR),
	  doc_site(call,   Loc, Cond, CP,  NP, AType, Status, DocSt, CPR),
	  doc_site(answer, Loc, Cond, AP,  NP, AType, Status, DocSt, APR),
	  doc_site(global, Loc, Cond, NGP, NP, AType, Status, DocSt, NGPR),
	  UsageR = [HeaderR, HeadR, itemize_minus([DescR, DPR, CPR, APR, NGPR])]
        ).

fmt_head_descriptor(P, Type, Standard, HeadR) :-
	( P=_F/_A ->
	    R1 = []
	; assrt_type_text(Type, _Text, Prefix, Postfix),
	  format_to_string("~w", [P], PS),
	  R1 = [string_esc(Prefix), tt(string_esc(PS)), string_esc(Postfix)]
	),
	fmt_standard(Standard, StandardR),
	( StandardR = [] ->
	    HeadR0 = R1
	; HeadR0 = left_and_right(R1, StandardR)
	),
	( HeadR0 = [] ->
	    HeadR = HeadR0
	; HeadR = [HeadR0, raw_nl]
	).

fmt_standard(iso, R) :- !,
	R = iso("").
fmt_standard(_Standard, []).

gen_usage_header(N, Multiple, R) :-
	( Multiple = 1 ->
	    format_to_string("Usage ~w:", N, UsageStr),
	    R = [p(""), bf(string_esc(UsageStr)), string_esc(" ")]
	; Multiple = 0 ->
	    R = [p(""), bf(string_esc("Usage:")), string_esc(" ")]
	; R = []
	).

allvars([]).
allvars([H|T]) :-
	var(H),
	allvars(T).

%% ---------------------------------------------------------------------------
:- use_module(library(assertions(assertions_props)), [assrt_type/1]).

:- pred assrt_type_text(Type,Text,Prefix,Postfix) 
	: assrt_type * string * string * string
	# "@var{Text} is an appropriate text for the header for
           @var{Type}.  Same for @var{Prefix} and @var{Postfix}".

assrt_type_text(pred,    "PREDICATE",   "",    "") :- !.
assrt_type_text(compat,  "PREDICATE",   "",    "") :- !.
assrt_type_text(calls,   "PREDICATE",   "",    "") :- !.
assrt_type_text(success, "PREDICATE",   "",    "") :- !.
assrt_type_text(comp,    "PREDICATE",   "",    "") :- !.
%% assrt_type_text(func,    "FUNCTION",    "",    "") :- !.
assrt_type_text(prop,    "PROPERTY",    "",    "") :- !.
assrt_type_text(regtype, "REGTYPE",     "",    "") :- !.
assrt_type_text(decl,    "DECLARATION", ":- ", ".") :- !.
assrt_type_text(modedef, "MODE",        "",    "") :- !.
assrt_type_text(entry,   "ENTRY POINT", "",    "") :- !.
%% This one just for undocumented reexports:
assrt_type_text(udreexp, "(UNDOC_REEXPORT)",     "",    "") :- !.
assrt_type_text(_,       "UNKNOWN",     "",    "") :- !.

%% ---------------------------------------------------------------------------
:- pred doc_site/9 # "Generates documentation for each program point
   (call, exit, ...) of a predicate.".

doc_site(_T, _Loc, _Cond, Props, _P, _Type, _Status, _DocSt, R) :-
	Props = [],
	!,
	R = [].
doc_site(T, Loc, Cond, Props, P, Type, Status, DocSt, R) :-
	site_text(T, Cond, Type, Status, Text, Bullet),
	!,
	fmt_site_begin(Text, Bullet, BeginR),
	doc_properties(Props, Loc, P, DocSt, PropsR),
	R = [BeginR, PropsR, raw_nl].
doc_site(T, Loc, _Cond, Props, P, _Type, _Status, _DocSt, R) :-
	R = [],
	warning_message(Loc,
	    "error while formatting ~w properties ~w for predicate ~w",
	    [T, Props, P]).

fmt_site_begin(Text, bullet, BeginR) :-
	BeginR = [item(""), em(string_esc(Text))].
fmt_site_begin(Text, nobullet, BeginR) :-
	BeginR = [raw_nl, raw_nl, em(string_esc(Text)), raw_nl].

site_text(compat, _Cond, pred, Status, Text, bullet) :-
%% Special case for true/trust pred, compat properties:
	(Status = true ; Status = trust),
	!,
	Text = "Calls should, and exit will be compatible with:".
site_text(compat, _Cond, _Type, Status, Text, bullet) :-
	!,
	status_text_infix(Status, SText),
	list_concat(["Call and exit", SText, "compatible with:"], Text).
site_text(T, Cond, Type, Status, Text, Bullet) :-
	status_text_prefix(Type, T, Cond, PText, Bullet),
	status_text_mode(Status, Type, T, MText),
	prog_point_text(T, PPText),
	!,
	list_concat([PText, MText, PPText], Text).

status_text_infix(trust,   " are ").
status_text_infix(true,    " are ").
status_text_infix(false,   " are not ").
status_text_infix(check,   " should be ").
status_text_infix(checked, " are ").

status_text_prefix(modedef, _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(pred,    _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(calls,   _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(decl,    _,      _,
	           "The following properties", bullet) :- !.
status_text_prefix(_,       call,   _,
	           "If the following properties", bullet).
status_text_prefix(_,       answer, full,
	           "then the following properties", nobullet).
status_text_prefix(_,       answer, empty,
	           "The following properties", bullet).
status_text_prefix(_,       global, full,
	           "then the following properties", nobullet).
status_text_prefix(_,       global, empty,
	           "The following properties", bullet).

%% Introduced special case for guard
status_text_mode(_, modedef, _,    " are added ") :- !.
status_text_mode(_, success, call, " hold ") :- !.
status_text_mode(_, comp,    call, " hold ") :- !.
%% Introduced special case for true/trust pred.
status_text_mode(trust,   pred, call, " should hold ") :- !.
status_text_mode(trust,   _,    _,    " hold ").
status_text_mode(true,    pred, call, " should hold ") :- !.
status_text_mode(true,    _,    _,    " hold ").
status_text_mode(false,   _,    _,    " do not hold ").
status_text_mode(check,   _,    _,    " should hold ").
status_text_mode(checked, _,    _,    " are proved to hold ").

prog_point_text(call,   "at call time:").
prog_point_text(answer, "upon exit:").
prog_point_text(global, "globally:").

%% ---------------------------------------------------------------------------
:- pred doc_properties/5
# "Generates documentation for a conjunction (list) of properties.".

doc_properties([], _Loc, _P, _DocSt, []) :- !.
doc_properties([Prop|Props], Loc, P, DocSt, [PropR|PropsR]) :- !,
	doc_property(Prop, Loc, P, DocSt, PropR),
	doc_properties(Props, Loc, P, DocSt, PropsR).
doc_properties(Prop, Loc, P, DocSt, PropR) :-
	doc_property(Prop, Loc, P, DocSt, PropR).

doc_property(true, _Loc, _P, _DocSt, PropR) :- !,
	PropR = nop.
doc_property(Prop, Loc, P, DocSt, PropR) :-
	( prop_format(DocSt, Prop, Loc, PM, BasicFormat, VarDict) ->
	    ( doc_opt('-literalprops', DocSt) ->
	        empty_doctree(DocR)
	    ; DocR = BasicFormat
	    ),
	    ( PM = user(FullPath),
	      main_filename(FullPath, UFName) ->
	        NPM = user('...'/UFName)
	    ; NPM = PM
	    )
	; NPM = undefined,
	  DocR = undefined,
	  VarDict = [],
	  warning_message(Loc, "unknown property ~w in assertion for ~w", [Prop, P]),
	  ttyflush
	),
	fmt_property(DocSt, Prop, NPM, DocR, VarDict, PropR).

%% -literalprops -nopropnames -noundefined -nopropsepln

fmt_property(DocSt, Prop, PM, DocString, VarDict, PropR) :-
	( ( doc_opt('-literalprops', DocSt)
	  ; doctree_is_empty(DocString)
	  ; DocString == undefined
	  ) ->
	    fmt_propcode(PM, Prop, DocSt, PropLitR)
	; fill_vardict(VarDict),
	  PropLitR = DocString
	),
	( ( DocString\==undefined
	  ; doc_opt('-noundefined', DocSt)
	  ) ->
	    UndefR = []
	; UndefR = string_esc(" (undefined property)")
	),
	( ( DocString==undefined
	  ; doc_opt('-nopropnames', DocSt)
	  ; doc_opt('-literalprops', DocSt)
	  ) ->
	    DescR = []
	; functor(Prop, F, A),
	  Desc = F/A,
	  fmt_propcode(PM, Desc, DocSt, Desc2),
	  DescR = [string_esc(" ("), Desc2, string_esc(")")]
	),
	!,
	( doc_opt('-nopropsepln', DocSt) ->
	    PropR = [string_esc(" "), PropLitR, UndefR, DescR, string_esc(".")]
	; PropR = [linebreak, left_and_right([PropLitR, raw_nl], [UndefR, DescR])]
	).
fmt_property(_DocSt, Prop, PM, _DocString, _VarDict, PropR) :-
	PropR = [],
	error_message("could not format property ~w:~w", [PM, Prop]).	

fmt_propcode(PM, Prop, DocSt, R) :-
	% TODO: the module is ignored for indexing; wrong
	format_to_string("~w", [Prop], Ref),
	( doc_opt('-propmods', DocSt) ->
	    format_to_string("~w:~w", [PM, Prop], S)
	; S = Ref
	),
	( doc_opt('-nopropuses', DocSt) ->
	    R = [string_esc(S)]
	; R = [idx_env(use, prop, localnum_label(_), string_esc(Ref), string_esc(S))]
	).

fill_vardict([]).
fill_vardict([X=V|Ds]) :-
	format_to_string("~w", [V], S),
	X = var([string_esc(S)]),
	fill_vardict(Ds).

%% ---------------------------------------------------------------------------
:- pred doc_description/5
# "Generates documentation for a predicate or prop description.".

doc_description(Desc, _Loc, P, _DocSt, DescR) :-
	Desc = [],
	( P = F/A -> true
	; functor(P, F, A)
	),
	!,
	DescR = [].
%%	note_message("no comment found for usage in ~w/~w",[F,A]).
doc_description(Desc, Loc, _P, DocSt, DescR) :-
	parse_docstring_loc(DocSt, Loc, Desc, DescR0),
	DescR = [item(""), em(string_esc("Description:")), string_esc(" "), DescR0].

%% ---------------------------------------------------------------------------
:- pred prop_format(DocSt, Prop, Loc, PM, BasicFormat, VarDict)

# "Given a property @var{Prop} (which appears in an assertion), a
   string is generated which expresses in words the meaning of the
   property. In order to be able to do this, a documentation string
   must be provided (in a standard declaration) in the place where the
   property itself is defined and documented. Such property
   definitions may be in the same module or exported by any module
   visible -- through @pred{use_module/1} -- to the module being
   documented.  Some complication comes from the fact that the
   documentation should be generated in terms of the variables
   appearing in @var{Prop}, rather than the ones in the original
   definition. The output is @var{BasicFormat} (containing free
   variables in the places where the variables names should appear)
   and a list of pairs @var{VarDict} relating the free variables with
   the (possibly repeated) variable names. @var{PM} is the module in
   which the property is defined.".

prop_format(DocSt, Prop, Loc, PM, BasicFormat, VarDict) :-
	nonvar(Prop),
	% Get assertion
	assertion_read(Prop, PM, _PStatus, PType, NAss, PDict, _, _, _),
	propfunctor(PType), %% prop, ...
	%% Should add also ~imports(M,AM,F,A), but, since this is flagged 
	%% during normalization, here we use whatever we can find.
	% Get comment field
	assertion_body(Prop, _DP, _CP, _AP, _GP, Comment, NAss),
	% Rewrite the comment
	maybe_remove_full_stop(DocSt, Comment, Comment2),
	parse_docstring_loc(DocSt, Loc, Comment2, Comment3),
	doctree_putvars(Comment3, DocSt, PDict, VarDict, BasicFormat).

maybe_remove_full_stop(DocSt, DocString, DocString2) :-
	( ( doc_opt('-nopropsepln', DocSt),
	    list_concat([NewDocString, "."], DocString)
	  ) -> %% Just to make it nicer: get rid of final full stop.
	    DocString2 = NewDocString
	; DocString2 = DocString
	).

%% ---------------------------------------------------------------------------
:- pred bind_dict_varnames(Dict) # "Binds the variables in @var{Dict}
   to the corresponding names (i.e., the names that appeared in the
   source during read.".

bind_dict_varnames([]).
bind_dict_varnames([VarName=Var|Rest]) :-
	VarName=Var,
	bind_dict_varnames(Rest).

%% ---------------------------------------------------------------------------
:- pred fix_var_arg_names(H, Loc, NH) # "In both @var{NH} and @var{H} the
   arguments of @var{H} which are vars are replaced with the name of
   their argument position, i.e.,
   @tt{fix_var_arg_names(p(X,a),p('Arg1',a)}. However, if all
   arguments of @var{H} are originally free variables, then @var{NH}
   is of the form @tt{F/A}, where @tt{F} is the principal functor of
   @var{H} and @tt{A} its arity. An exception to this occurs when
   there is a @pred{doc/2} declaration whose first argument is a
   predicate descriptor specifying argument names: these names are
   used in this case instead of 'ArgN'.".

fix_var_arg_names(H, Loc, NH) :-
	functor(H,  F, A),
	functor(CH, F, A),
	get_doc_field_dict(pred(CH), _, Dict),
	CH =.. [_|Args],
	( all_var_args(Args) ->
	    bind_dict_varnames(Dict),
	    do_fix_var_arg_names(H, NH, CH)
	; warning_message(Loc, "nonvariable argument(s) in comment head ~w, "
		|| "variable names ignored", [CH]),
	  do_fix_var_arg_names(H, NH, [])
	).
fix_var_arg_names(H, _Loc, NH) :-
	do_fix_var_arg_names(H, NH, []).

all_var_args([]).
all_var_args([H|T]) :-
	var(H),
	all_var_args(T).

do_fix_var_arg_names(H, NH, CH) :-
	functor(H, F, A),
	fix_A_var_arg_names(A, H, Allvars, CH),
	( Allvars == false -> NH=H
	; NH=F/A
	).

fix_A_var_arg_names(0, _H, _Allvars, _CH) :- !.
fix_A_var_arg_names(A, H,  Allvars,  CH) :-
	A > 0,
	NA is A-1,
	arg(A, H, Arg),
	( var(Arg) ->
	    ( CH == [] ->
	        number_codes(A, AS),
		atom_codes(AA, AS),
		atom_concat(['Arg', AA], Arg)
	    ; arg(A, CH, Arg)
	   %% Allvars=false 
	    )
	; Allvars=false
        ),
	fix_A_var_arg_names(NA, H, Allvars, CH).

% ---------------------------------------------------------------------------

% Format the input list of doctree as a comma-separated, dot-ended
% doctree.
fmt_commas_period([], []) :- !.
fmt_commas_period([A], [R]) :- !,
	R = [A, string_esc(".")].
fmt_commas_period([A|As], [R|Rs]) :-
	R = [A, string_esc(", ")],
	fmt_commas_period(As, Rs).

% ===========================================================================

:- doc(section, "Compute Global References for a Module").
% TODO: This could be optimized! It is the same for every module (profile it!)

:- export(autodoc_compute_grefs/3).
:- pred autodoc_compute_grefs/3 # "Compute the globally resolved
references (including bibliography)".
% (do not call for components)
autodoc_compute_grefs(Backend, Mod, Opts) :-
	verbose_message("{Globally resolving references", []),
	new_docstate_no_src(Backend, Mod, Opts, DocSt),
	compute_refs_and_biblio(DocSt),
	verbose_message("}", []).

% ===========================================================================

:- doc(section, "doctree Translation and Output").

:- export(autodoc_translate_doctree/3).
:- pred autodoc_translate_doctree/3 # "Translate the doctree using the specific
backend".
autodoc_translate_doctree(Backend, Opts, Mod) :-
	verbose_message("{(Backend ~w) Translating intermediate representation of ~w", [Backend, Mod]),
	new_docstate_no_src(Backend, Mod, Opts, DocSt),
	absfile_for_subtarget(Mod, Backend, cr, OutFile),
	doctree_restore_and_write(Mod, OutFile, DocSt),
	% TODO: This generates the infoindex if necessary; generalize for other formats
	( doc_doing_mainmod(DocSt),
	  Backend = texinfo ->
	    fmt_infodir_entry2(DocSt, Mod)
	; true
	),
	verbose_message("}", []).

% ---------------------------------------------------------------------------

fmt_infodir_entry2(DocSt, Mod) :-
	% TODO: Why don't we write the file directly?
	% Write to a '*dir.info' file
	% (this is later renamed to .infoindex; cannot use infoindex
        %  here because Mod is the same as for the main file,
        %  which makes doctree_to_file overwrite some important
        %  intermediate files)
	infodir_base(Mod, ModInfodir),
	%
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(ModInfodir, Backend, cr, OutFile),
	%
	docstate_set_currmod(DocSt, ModInfodir, DocSt1),
	doctree_restore_and_write_norefs(ModInfodir, OutFile, DocSt1).

% ---------------------------------------------------------------------------

doctree_restore_and_write(Mod, OutFile, DocSt) :-
	% Now that (global) references are computed, restore the
	% doctree, references, and translate.
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(Mod, Backend, dr, RFile),
	doctree_restore(RFile, R),
	try_finally(open(OutFile, write, OS),
	            doctree_prepare_refs_translate_and_write(R, DocSt, OS),
		    close(OS)).

% TODO: For infoindex generation. Is it worth a special case?
doctree_restore_and_write_norefs(Mod, OutFile, DocSt) :-
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(Mod, Backend, dr, RFile),
	doctree_restore(RFile, R),
	try_finally(open(OutFile, write, OS),
	            doctree_translate_and_write(R, DocSt, OS),
		    close(OS)).

% ===========================================================================

:- doc(section, "Finish Document and Generate Alternative Output").

:- multifile autodoc_finish_hook/1.

:- export(autodoc_finish/1).
autodoc_finish(Backend) :-
	autodoc_finish_hook(Backend).

:- multifile autodoc_gen_alternative_hook/2.

:- export(autodoc_gen_alternative/2).
autodoc_gen_alternative(Backend, Alt) :-
	autodoc_gen_alternative_hook(Backend, Alt).

% ===========================================================================

:- doc(section, "Versions and Operations on Versions").

just_a_version(Version) :- nonvar(Version), Version \== [].

version_format(version(V*SV+P, Y/M/D),          V, SV, P, Y, M, D, [], [], [],
	    []).
version_format(version(V*SV+P, Y/M/D, []),      V, SV, P, Y, M, D, [], [], [],
	    []).
version_format(version(V*SV+P, Y/M/D, H:N*S+Z), V, SV, P, Y, M, D, H,  N,  S,
	    Z).

:- export(version_patch/2).
version_patch(V, VPatch) :-
	version_format(V, _, _, VPatch, _, _, _, _, _, _, _).

:- export(version_date/2).
version_date(Version, Date) :-
	( version_format(Version, _, _, _, Year, Month, Day, _, _, _, _) ->
	    Date = Year/Month/Day
	; fail
	).

:- export(version_numstr/2).
version_numstr(Version, Str) :-
	( version_format(Version, Ver, Sub, Patch, _, _, _, _, _, _, _) ->
	    format_to_string("~w.~w#~w", [Ver, Sub, Patch], Str)
	; fail
	).

version_string(Version, Str) :-
	( Version = version(Ver*Sub+0, Date)
	; Version = version(Ver*Sub+0, Date, [])
	),
	!,
	format_to_string("~w.~w (~w)", [Ver, Sub, Date], Str).
version_string(version(Ver*Sub+0, Date, H:M*S+Z), Str) :-
	!,
	format_to_string("~w.~w (~w, ~w:~w:~w ~w)",
	                 [Ver, Sub, Date, H, M, S, Z], Str).
version_string(Version, Str) :-
	( Version = version(Ver*Sub+Patch, Date)
	; Version = version(Ver*Sub+Patch, Date, [])
	),
	!,
	format_to_string("~w.~w#~w (~w)",
	                 [Ver, Sub, Patch, Date], Str).
version_string(version(Ver*Sub+Patch, Date, H:M*S+Z), Str) :-
	!,
	format_to_string("~w.~w#~w (~w, ~w:~w:~w ~w)",
	                 [Ver, Sub, Patch, Date, H, M, S, Z], Str).
version_string(Version, Str) :-
	warning_message("unrecognized version format: ~w", [Version]),
	Str = "".

% ===========================================================================

:- doc(section, "Auxiliar Definitions").

:- pred eliminate_duplicates(X,Y) # "@var{Y} is @var{X} whre
   duplicated elements has been removed".

eliminate_duplicates(X, Y) :-
	eliminate_duplicates_(X, [], Y).

eliminate_duplicates_([],    _,    []).
eliminate_duplicates_([H|T], Seen, NT) :-
	member(H, Seen),
	!,
	eliminate_duplicates_(T, Seen, NT).
eliminate_duplicates_([H|T], Seen, [H|NT]) :-
	eliminate_duplicates_(T, [H|Seen], NT).

% ===========================================================================

:- doc(section, "Tasks and Issues (private)").

:- doc(subsection, "Future Work").

:- doc(bug, "Currently, the input from autodoc is a
   SETTINGS.pl. Currently, this is quite limited since there can exist
   only one SETTINGS.pl loaded at a time, and it interact with other
   tools (e.g., lpmake)").

:- doc(bug, "in Ciao tree we need to automatically create local
   subset of (clip) bibtex files to make manuals standalone.").

:- doc(bug, "entry declarations documented as 'if'?.").

:- doc(bug, "htmlview command must be configurable.").

:- doc(bug, "documentation of exceptions.").

:- doc(bug, "Actually, texi, dvi, etc. should all be generated in 
   subdirectories, containing their figures (as well as generating 
   a tar file).").

:- doc(bug, "Separate emacs mode from ciao.").

:- doc(subsection, "Known Bugs").

:- doc(bug, "Make sure that richer paths are valid in @@image
   command").

:- doc(bug, "Make sure that we only eliminate characters [',:-] from
   node names, but from section names").

:- doc(subsection, "Other (Unclassified) Bugs").
% (Before my changes --JFMC)

:- doc(bug, "Compression, generation of tar, etc., should be switchable 
   by format (easy now!).").

:- doc(bug, "Types and props should have a link to their
   definitions (also in pdf!). This would be a mess in info, but in
   info it is not necessary: you can go with C-c tab or search... ").

:- doc(bug, "C-u M-x info a INSTALL and emacs mode (also in ciao)").

:- doc(bug, "single-sided versus double-sided").

:- doc(bug, "Document classes: see mess from Angel").

:- doc(bug, "support for path aliases and the CIAOALIASPATH variable").

:- doc(bug, "BTW: Now that you've installed a new version of
   pl2texi, why don't I get messages like:

   VERY DEPRECATED: @@begin@{verbatim@}, use @@begin@{pre@} instead

   And another question:
   Have you changed the braveclean to remove .css files yet? (Or do you want
   me to have a look at it?)
   ").

:- doc(bug, "uninstall needs to be debugged (the ifs in Makefile.skel)").

:- doc(bug, "fix problems because of eps figures in pdf").

:- doc(bug, "lpdoc should sign its manuals").

:- doc(bug, "Reexported global predicate + assertions: only the
   assertions are documented...").

:- doc(bug, "Find a way that one can specify that a given assertion
   should not appear in the documentation (nodoc/1 a global
   property?). Also, the other way around.").

:- doc(bug, "Apparently parts cannot be referenced").

:- doc(bug, "The first paragraph (module comment) should be an
   'introduction' section, at least when there is going to be an
   interface description.").

:- doc(bug, "The ciao manual should have the version name in the
   file name! (i.e., ciao-0.9.info) ??? Yes, so that you can have
   several versions installed.").

:- doc(bug, "Estos dos son de las aserciones:

* Distinguir propiedades ""+"" (que tienen un argumento implícito de la
  computación) de el resto, ya que en la documentación ese argumento no
  se nombra.

* Hacer que el chequeador de tipos no se queje de las definiciones de
  los tipos predefinidos (term, int, ...) en el engine. (Mediante
  declaración?)

Específicos de lpdoc:

* Hacer que lpdoc pueda leer al principio un fichero de
  inicialización, por ejemplo para definir operadores (y así los modos
  salen bonitos).

* En vez de que haya un comando @@iso, que haya uno @@sign@{@} (o como se
  llame) tal que @@sign@{ISO@} haga lo que ahora hace @@iso (para que tenga
  más utilidad).

* Es necesario que un módulo pueda definir declaraciones para que, por
  ejemplo, data_facts pueda documentar :- data.

* Cambiar lpdoc para que los títulos de los índices no estén
  _capitalizados_: ""Library index"", etc.

* Hacer un comando @@flag@{Flag@} para nombrar prolog flags.  Interpretar
  el multifile define_flag/3 para documentar flags definidos en módulos.

* Bugs: en engine(atomic_basic) no aparecen los usos sin ningun campo (pero
  tienen modos).  Tambien pasa en engine(io_basic), donde ademas el uso
  de nl/0 no aparece.

").

:- doc(bug, "if an assertion is present for p, even if the
   predicate is reexported, it should be documented (no need for
   doinclude). See sockets.pl").

:- doc(bug, "if an imported type is redefined locally and then
   listed, all clauses appear").

:- doc(bug, "underscores in file names result in problems when
   including figures (a texinfo bug). Possible fix:

   \newcommand@{\fichero@}[1]@{\catcode\`\~=11%
            \catcode\`\_=11%
            \emph@{#1@}@}

   ").

:- doc(bug, "Mirando en lpdoc.pl cómo se hacía lo de poner el
   mensaje de ""Usage:"" para que salga en el man, he visto que haces

   :- use_module(library(iso_byte_char)).
 
   pero no pareces usar nada de ahí, también haces

   :- use_module(library(dcg(dcg_tr))).

   y creo que tampoco es necesario (sólo es necesario si hay meta-llamadas
   en las gramáticas).
").

:- doc(bug, "putting props in a prop should give errors?").

:- doc(bug, "Es esto un bug o yo no me entero?  Dada la declaracion 
   (en system.pl):  

   :- true pred umask(OldMask, NewMask)
        : (var(OldMask), var(NewMask), OldMask == NewMask)
       $=>$ (int(OldMask), int(NewMask))
        # ""Gets the process file creation mask without changing it."".

   @{WARNING (autodoc): unknown property int(OldMask) in assertion for 
   umask(OldMask,NewMask)@} 

   Tampoco funciona si pongo int * int.

   ").

:- doc(bug, "El fichero m_ciaopp.pl tiene:
   :- doc(doinclude,lub/1).
   y la documentacion de lub/1 tiene:
   :- doc(lub/1,
	""This predicate handles a flag that indicates
	 whether to lub abstract substitutions computed for each clause 
         during analysis."").
   :- pred lub/1 : var $=>$ ok_ans 
	# ""Mode for querying the current flag value"" .
   :- pred lub/1 : @{ground,ok_ans@} $=>$ ok_ans 
	# ""Mode for setting the current flag value"" .

   pero el texto de doc(lub/1,...
   no sale en el manual. 
   Salen solo los usages correspondientes a las dos preds ...").

:- doc(bug, "Change granularity: generate one section 
   for a given library (at least in HTML).").

:- doc(bug, "Check that @@pred etc. state arity!").

:- doc(bug, "Warning: '@@verbatim' is deprecated, use '@@pre' instead.
   In the new latex it is 'alltt'.").

:- doc(bug, "Una solucion posible es que lpdoc cambie
   automaticamente : @@subsection@{My section@} por @@subsection@{My
   section (current_doc)@}.").

:- doc(bug, "usage,nil or something like that so no usage section
   is generated.").

:- doc(bug, "List of local opts: :- doc(localopts,['-nochangelog']).").

:- doc(bug, "if an imported predicate is redefined, the local
   version should always be the one documented!!!!").

:- doc(bug, "we need macros").

:- doc(bug, "ops and decls from assertions should not be documented").

:- doc(bug, "Conditional inclusion, in order to make several types
   of manuals from a single file, e.g., :- doc(doinclude(refmanual),p/3)
   and 'refmanual' is an option passed to lpdoc (in SETTINGS).  ").

:- doc(bug, "assrt_lib Make sure .asr file has same permissions as 
   .pl file!!!!").

:- doc(bug, "Another list (in another Makefile variable) called
   APPENDICES lists files that should go in appendices").

:- doc(bug, "prolog flag definitions should be documented").

:- doc(bug, "Documentation needs to be completed: assertions,
   etc.").

:- doc(bug, "Should install in more standard places (info, man, etc.)").

:- doc(bug, "Should support texinfo @@dircategory and the
   install-info method.").

:- doc(bug, "Probar a poner abstract donde ahora va module, despues
   interface y despues module como introduccion.  En info, interface va
   despues de abstract. Ah, y que en info el summary y el interface van
   en el mismo nodo.").

:- doc(bug, "local properties (not exported) used in a predicate
   which is exported: documented correctly, but an error message is
   issued. CHECK!").

:- doc(bug, "ppm/jpg conversion fails for certain eps files").

:- doc(bug, "Documentation for main file should produce *global*
   changelog.").

:- doc(bug, "Arithmetic vs. term typing (but only if -nomodes)").

:- doc(bug, "(Implementation defined ?)").

:- doc(bug, "The '@@@{' and '@@@}' characters must be eliminated from
   the files generated by bibtex. Also check out what is best:
   '@@dotless@{i@}' or '@@dotless i'.").

:- doc(bug, "Authors should go into the index. The global/concept
   index, or a separate 'authors' index. This may require identifying
   author address lines separately. Subauthor (or address): these
   lines would not be included when documenting chapters.").

%%% 1. Relatively easy and nice (i.e., good for having a good time):

:- doc(bug, "How about using :- doc(+/2,""), where + is an
   operator (?) ").

:- doc(bug, "Local options in file -- something like "||
	":- doc(options,...).").

:- doc(bug, "Assertions should describe which database predicates are read
   and written by a given predicate.").

:- doc(bug, "SICStus compatibility library!").

:- doc(bug, "HTML indices based on templates?").

:- doc(bug, "Could generate automatically a 'changes' section for
   the htmlindex... (nice for people to know if they want to download
   a new version).").

:- doc(bug, "in assrt_lib: avoid reloading assertions from
   libraries (or put a switch). ").

:- doc(bug, "figures should not be in the doc directory?!? If not,
   put warning in manual.").

:- doc(bug, "when including support.pl in bibutils doc,
   line_count/2 fails instead of aborting... CHECK").

:- doc(bug, "Add commands to change chapter number?").

:- doc(bug, "'.' still appears sometimes as first character of a
   line in man format: problematic!").

:- doc(bug, "Esto no es realmente importante, pero resulta que los
   predicados cuyo nombre se compone de simbolos (e.g (=:=)/2 ),
   deberian aparecer en el indice como '=:= /2' (el nombre separado
   del '/'), ya que unido se parsea de otra forma en Prolog (asi
   tambien aparece en el manual de SICStus).").

:- doc(bug, "Check out haskell_doc.el!.").

%%% 2. More complicated (i.e., real work):

:- doc(bug, "Almost all error messages should give line numbers...").

:- doc(bug, "Add 'INCLUDES' to settings: creates dependencies from
   included files. But complex: has to be done for each
   component... => lpdoc generates a file listing 
   the includes!...").

:- doc(bug, "Module usages should say: 'assertions/assrt_lib', but this 
   is too hard to do until we get rid of the Makefile.").

:- doc(bug, "Include a directory 'ciaolib', in which, every now and
   then (on demand?, when compiling?, when installing?) the ciao
   libraries used by lpdoc are copied (this makes it
   standalone). This can now be done quite well with the new
   distribution method. Also, do autoload depending on format?").

:- doc(bug, "Should have @@includedoc@{predname/arity@} command,
   which includes inline the documentation of a predicate. Also, a
   command to prevent the automatic generation of the standard
   sections with exported predicates, etc., so that it can all be done
   by hand.").

:- doc(bug, "Should have a real intermediate language from which
    all the backend stuff is done.").

:- doc(bug, "Disjunctions in properties not supported yet.").

%%% 3. Not really easy to fix (e.g., it is a bug in another tool):

:- doc(bug, "in basic_props, properties appear twice -- why?
   Because it includes itself!!! Discuss with Daniel...").

:- doc(bug, "In HTML, @@index@{patata@} should be a hyperlink to where
   @@concept@{patata@} appears.").

:- doc(bug, "Could also generate SGML and use sgml-tools in Linux
   to generate the other formats, but the format looks somewhat
   limited. There is a converter to rtf (windows help) though.").

:- doc(bug, "Should somehow generate rtf").

:- doc(bug, "Resulting info files (on-line versions) are still not
   very good regarding references (but not so easy to fix, because of
   limitations of info).").

:- doc(bug, "@@includedef should do a real verbatim... (pity that
   texinfo is so weird with this...").

:- doc(bug, "Idea: for each <module>.pl, produce a <module>_help.pl
   which extends a multifile predicate help/2 which defined help text
   for each predicate and module. <module>_help.pl would be loaded in
   the top level, but not in the executables. BUT This is covered
   however nicely now by word-help, and would slow down loading in the
   top level.").

