:- module(autodoc_state, [], [dcg, assertions, regtypes, basicmodes, fsyntax]).

:- doc(title, "Internal State for Documentation Generation").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines the internal state of the
   documentation generation (for a single module).").

% ---------------------------------------------------------------------------

%% Order below is still important (at least in current Ciao version):

% TODO: Refine
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

:- doc(section, "Documentation Options").

:- export(supported_option/1).
:- prop supported_option(Option) 

	# "@tt{Option} is a supported documentation option.".

% TODO: Duplicated in lpdoc/lib/SETTINGS_schema.pl
supported_option(verbose).
supported_option(no_bugs).
supported_option(no_authors).
supported_option(no_stability).
supported_option(no_version).
supported_option(no_versioned_output).
supported_option(no_changelog).
supported_option(no_patches).
supported_option(modes).
supported_option(head_props).
supported_option(literal_props).
supported_option(no_propnames).
supported_option(no_undefined).
supported_option(no_propsepln).
supported_option(no_biblio).
supported_option(no_sysmods).
supported_option(no_engmods).
supported_option(no_isoline).
supported_option(propmods).
supported_option(no_propuses).
supported_option(shorttoc).
supported_option(regtype_props).
supported_option(onesided).
supported_option(no_math).

:- export(option_comment/2).
:- pred option_comment(Option,Text) 

	=> supported_option * string

        # "@var{Option} is a documentation option which is
          supported. @var{Text} describes the effect of selecting that
          option. Currently supported options are:

@includedef{option_comment/2}          
          ".

option_comment(verbose,        "Verbose output (good for debugging).        ").
option_comment(no_bugs,      "Do not include information on bugs.         ").
option_comment(no_authors,   "Do not include author names.                ").
option_comment(no_stability, "Do not include stability comment.           ").
option_comment(no_version,   "Do not include version information.         ").
option_comment(no_versioned_output,
                               "Do not include version in the output name.  ").
option_comment(no_changelog, "Do not include change log.                  ").
option_comment(no_patches,   "Do not include comments for patches.        ").
%% @tt{modes} and @tt{headprops} are used by normalizer!
option_comment(modes,        "Do not translate modes and their arguments
	                        (except for properties)                     ").
option_comment(head_props,   "Do not move head properties to body.        ").

option_comment(literal_props,"Do not use text to document properties.     ").
option_comment(no_propnames, "Do not include property names in prop text. ").
option_comment(no_undefined, "Do not signal undefined properties in text. ").
option_comment(no_propsepln, "Do not put each property in a separate line.").

option_comment(no_biblio,    "Do not include a bibliographical 'References' appendix.").
option_comment(no_sysmods,   "Do not include system modules in list of 
                                libraries used.").
option_comment(no_engmods,   "Do not include system engine modules in list 
                                of libraries used.").
option_comment(no_isoline,   "Do not include *textual* description that a 
                                given usage conforms to the ISO standard.").
option_comment(propmods,    "Include module name to which props belong.").
option_comment(no_propuses, "Do not Include property uses (from assertions) in indices.").
option_comment(shorttoc,    "Produce shorter table of contents (no entries
                                for individual defs of preds, props, etc.).").
option_comment(regtype_props,"Include in the doc for regtypes the global
                                prop stating that they are indeed regtypes.").
option_comment(onesided,    "For printing on one side (default is two).").
option_comment(no_math,      "Disable mathematical environments.").

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

% ===========================================================================

:- doc(section, "Documentation State for a Module").
% This is the (partial) state of the generation process for the
% current component.

:- use_module(library(dict)).

% TODO: refine
:- export(docstate/1).
:- regtype docstate/1.
docstate(docstate(Backend, Name, Opts, MVarDic, I)) :-
	backend_id(Backend),
	atom(Name),
	list(Opts,supported_option),
	dictionary(MVarDic), % to keep custom data
	filename(I).

:- export(docst_backend/2).
docst_backend(DocSt, Backend) :-
	DocSt = docstate(Backend, _, _, _, _).

:- export(docst_currmod/2).
docst_currmod(DocSt, Name) :-
	DocSt = docstate(_, Name, _, _, _).

:- export(docst_set_currmod/3).
docst_set_currmod(DocSt0, Name, DocSt) :-
	DocSt0 = docstate(Backend, _,    Opts, MVarDic, I),
	DocSt  = docstate(Backend, Name, Opts, MVarDic, I).

:- export(docst_opts/2).
docst_opts(DocSt, Opts) :-
	DocSt = docstate(_, _, Opts, _, _).

:- export(docst_set_opts/3).
docst_set_opts(DocSt0, Opts, DocSt) :-
	DocSt0 = docstate(Backend, Name, _,    MVarDic, I),
	DocSt  = docstate(Backend, Name, Opts, MVarDic, I).

% 'mvar' is a dictionary for module-scoped global variables
%:- export(docst_mvar/2).
docst_mvardic(DocSt, MVarDic) :-
	DocSt = docstate(_, _, _, MVarDic, _).

%:- export(docst_set_mvar/3).
docst_set_mvardic(DocSt0, MVarDic, DocSt) :-
	DocSt0 = docstate(Backend, Name, Opts, _, I),
	DocSt  = docstate(Backend, Name, Opts, MVarDic, I).

:- export(docst_inputfile/2).
docst_inputfile(DocSt, I) :-
	DocSt = docstate(_, _, _, _, I).

%% ---------------------------------------------------------------------------

:- doc(subsection, "Create a New docstate").

:- export(docst_new_no_src/4).
% Create a new docstate (with no source loaded)
% TODO: keep OldLibs! remember to reset them
docst_new_no_src(Backend, Name, Opts, DocSt) :-
	DocSt = docstate(Backend, Name, Opts, _, ''),
	%
        setup_libpaths(DocSt, _). % TODO: Necessary?

% Create a new docstate and load the source
:- export(docst_new_with_src/6).
docst_new_with_src(Backend, FileBase, SourceSuffix, Opts, DocSt, OldLibs) :-
	DocSt = docstate(Backend, Name, Opts, _, I),
	%
	setup_libpaths(DocSt, OldLibs),
	%
	( SourceSuffix = 'lpdoc' ->
	    % TODO: incomplete and probably incorrect
	    I = '',
	    % TODO: This is a really simple and incomplete version for .lpdoc as a source!
	    atom_codes(FileBase, FBC),
	    append("@include{"|| FBC, ".lpdoc}", Command),
	    parse_docstring_loc(DocSt, _Loc, Command, ContentR),
	    ModuleR = [% TODO: first chapter line is not backend-agnostic, fix
		       raw("@chapter "), raw(FBC), raw("\n\n"),
		       ContentR],
	    %
	    get_name(FileBase, Name),
	    docst_mvar_lookup(DocSt, modinfo, modinfo(Name, FileBase)),
	    %
	    docst_mvar_lookup(DocSt, plain_content, ModuleR),
	    ModuleType = plain,
	    docst_mdata_assertz(modtype(ModuleType), DocSt),
	    docst_mvar_lookup(DocSt, modtype, ModuleType) % TODO: redundant
	; load_source(FileBase, SourceSuffix,
	    Name, M, I, Base, Dir,
	    Opts),
	  % TODO: M (the Prolog module name) and Name may be different...
	  docst_mvar_lookup(DocSt, modinfo, modinfo(M, Base)),
	  docst_mvar_lookup(DocSt, dir, dir(Dir)),
	  detect_modtype(DocSt, ModuleType),
	  docst_mdata_assertz(modtype(ModuleType), DocSt),
	  docst_mvar_lookup(DocSt, modtype, ModuleType) % TODO: redundant
	).

:- use_module(library(make(make_rt)), [get_name/2]).

%% ---------------------------------------------------------------------------

% Create a state for a subfile
:- export(docst_new_sub/3).
docst_new_sub(DocSt0, SubSuffix, DocSt) :-
	docst_currmod(DocSt0, Name),
	get_subbase(Name, SubSuffix, SubName),
	docst_set_currmod(DocSt0, SubName, DocSt1),
	% some mvar need to be cleaned
	% TODO: generalize
	docst_mvar_replace(DocSt1, full_toc_tree, _, DocSt2),
	docst_mvar_replace(DocSt2, curr_toc_tree, _, DocSt3),
	docst_mvar_replace(DocSt3, nav, _, DocSt).

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
	docst_opt(lib_opts(LibPaths, SysLibPaths, PathAliasF), DocSt),
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

:- export(docst_message/2).
docst_message(Text, DocSt) :-
	docst_opts(DocSt, Opts),
	optional_message(Text, Opts).

:- export(docst_message/3).
docst_message(Text, Args, DocSt) :-
	docst_opts(DocSt, Opts),
	optional_message(Text, Args, Opts).

:- export(docst_opt/2).
docst_opt(Opt, DocSt) :-
	docst_opts(DocSt, Opts),
	member(Opt, Opts), !.

:- use_module(library(lists), [select/3]).

:- export(docst_currmod_is_main/1).
docst_currmod_is_main(DocSt) :-
	docst_currmod(DocSt, Name),
	get_mainmod(Name), !.

:- export(docst_no_components/1).
:- pred docst_no_components(DocSt) # "@var{DocSt} specify an empty list of components".
docst_no_components(_DocSt) :- all_component_specs([]).

:- export(docst_modname/2).
:- pred docst_modname(DocSt, ModName) # "@var{ModName} is the name of
   the module that we are documenting.".
docst_modname(DocSt, NDName) :-
	docst_currmod(DocSt, Name),
	( atom_concat(NDName, '_doc', Name) ->
	    true
	; NDName = Name
	).

% ======================================================================

:- doc(section, "Module-local Label Generator").
% (for anonymous reference labels)

:- data labcounter/2.

:- export(labgen_init/1).
labgen_init(DocSt) :-
	docst_currmod(DocSt, Name),
	assertz_fact(labcounter(0, Name)).

:- export(labgen_clean/1).
labgen_clean(DocSt) :-
	docst_currmod(DocSt, Name),
	retractall_fact(labcounter(_, Name)).

:- export(labgen_get/2).
labgen_get(DocSt, Label) :-
	docst_currmod(DocSt, Name),
	retract_fact(labcounter(N, Name)),
	number_codes(N, Label),
	N1 is N + 1,
	assertz_fact(labcounter(N1, Name)).

% ======================================================================

:- doc(section, "Module-scope Variables and Data").
% (Interface and serialization)

:- data docst_mdata/2.

:- export(docst_mvar_lookup/3).
docst_mvar_lookup(DocSt, K, V) :-
	docst_mvardic(DocSt, MVarDic),
	dic_lookup(MVarDic, K, V).

:- export(docst_mvar_replace/4).
docst_mvar_replace(DocSt0, K, V, DocSt) :-
	docst_mvardic(DocSt0, MVarDic0),
	dic_replace(MVarDic0, K, V, MVarDic),
	docst_set_mvardic(DocSt0, MVarDic, DocSt).

:- export(docst_mvar_get/3).
docst_mvar_get(DocSt, K, V) :-
	docst_mvardic(DocSt, MVarDic),
	dic_get(MVarDic, K, V).

:- export(docst_mdata_clean/1).
docst_mdata_clean(DocSt) :-
	docst_currmod(DocSt, Name),
	retractall_fact(docst_mdata(_, Name)).

:- export(docst_mdata_assertz/2).
docst_mdata_assertz(Entry, DocSt) :-
	docst_currmod(DocSt, Name),
	assertz_fact(docst_mdata(Entry, Name)).

:- export(docst_mdata_save/1).
docst_mdata_save(DocSt) :-
	docst_currmod(DocSt, Name),
	docst_backend(DocSt, Backend),
	absfile_for_subtarget(Name, Backend, rr, RefsFile),
	open(RefsFile, write, RefsOS),
	push_prolog_flag(write_strings, on),
	( % (failure-driven loop)
          docst_mdata(Entry, Name),
%	  display(user_error, refsentry(RefsFile, Entry)), nl(user_error),
	  % quote atoms so that it can be read back properly
	    writeq(RefsOS, Entry),
	    write(RefsOS, '.\n'),
	    fail
	; true
	),
	pop_prolog_flag(write_strings),
	close(RefsOS).

% TODO: missing docst_mdata_restore; it is put in docst_gdata_restore

:- use_module(library(write), [writeq/2,write/2]).
:- use_module(library(read), [read/2]).

% ======================================================================

:- doc(section, "Global-scope mdata").
% (Interface and serialization)

% Obtain all module-scope data that can be reached by following
% refs_link/1 items.

:- export(docst_gdata/3). % TODO: Remove
:- data docst_gdata/3.
% docst_gdata(Entry, Base, Name)
%   Entry is a datum appearing in Base

:- export(docst_gdata_query/2).
docst_gdata_query(DocSt, Entry) :-
	docst_currmod(DocSt, Name),
	docst_gdata(Entry, _Base, Name).

:- export(docst_gdata_query/3).
% (like docst_gdata_query/2 but returns the Base where the Entry comes from)
docst_gdata_query(DocSt, Base, Entry) :-
	docst_currmod(DocSt, Name),
	docst_gdata(Entry, Base, Name).

:- export(docst_gdata_restore/1).
docst_gdata_restore(DocSt) :-
%	docst_currmod(DocSt, Name),
	get_mainmod(MainName),
	docst_gdata_restore_(MainName, DocSt).

% (fails if the refs file does not exist)
docst_gdata_restore_(Base, DocSt) :-
	docst_backend(DocSt, Backend),
	absfile_for_subtarget(Base, Backend, rr, RefsFile),
	file_exists(RefsFile),
	!,
	docst_currmod(DocSt, Name),
	open(RefsFile, read, RefsOS),
	( repeat,
	  ( read(RefsOS, Entry) -> true ; fail ),
	  ( Entry = end_of_file ->
	     !
	  ; ( Entry = refs_link(Base2) ->
	        docst_gdata_restore_(Base2, DocSt)
	    ; assertz_fact(docst_gdata(Entry, Base, Name))
	    ),
	    fail
	  )
	; true
	),
	close(RefsOS).
docst_gdata_restore_(Base, _DocSt) :-
	throw(bug_no_rr_file_for(Base)).
        % Entry = sect([level(999)], "", string_esc("[ERROR-UNRESOLVED]")),
	% docst_currmod(DocSt, Name),
	% assertz_fact(docst_gdata(Entry, Name, Name)).

:- export(docst_gdata_clean/1).
docst_gdata_clean(DocSt) :-
	docst_currmod(DocSt, Name),
	retractall_fact(docst_gdata(_, _, Name)).

:- use_module(library(system), [file_exists/1]).

% ---------------------------------------------------------------------------

:- doc(section, "Global-scope mvar").
% TODO: at this moment, only bibliography

% TODO: docst_gvar_save and docst_gvar_restore are not good names...
:- export(docst_gvar_save/2).
docst_gvar_save(DocSt, MVars) :-
	% get_mainmod(MainName), % TODO: only valid for mainmod
	docst_currmod(DocSt, Name),
	docst_backend(DocSt, Backend),
 	absfile_for_subtarget(Name, Backend, gr, File),
	open(File, write, OS),
	push_prolog_flag(write_strings, on),
	save_mvar_entries(MVars, DocSt, OS),
	pop_prolog_flag(write_strings),
	close(OS).

:- export(docst_gvar_restore/2).
docst_gvar_restore(DocSt, MVars) :-
	get_mainmod(MainName),
	docst_backend(DocSt, Backend),
 	absfile_for_subtarget(MainName, Backend, gr, File),
	open(File, read, IS),
	restore_mvar_entries(MVars, DocSt, IS),
	close(IS).

save_mvar_entries([], _, _).
save_mvar_entries([V|Vs], DocSt, OS) :-
        save_mvar_entry(V, DocSt, OS),
        save_mvar_entries(Vs, DocSt, OS).

% Save mvar entry
save_mvar_entry(Var, DocSt, OS) :-
        ( docst_mvar_get(DocSt, Var, Value) ->
	    Term =.. [Var, Value],
	    writeq(OS, Term),
	    write(OS, '.\n')
	; throw(cannot_save(Var))
	).

restore_mvar_entries([], _, _).
restore_mvar_entries([V|Vs], DocSt, IS) :-
        restore_mvar_entry(V, DocSt, IS),
        restore_mvar_entries(Vs, DocSt, IS).

% Restore mvar entry
restore_mvar_entry(Var, DocSt, IS) :-
        docst_mvar_lookup(DocSt, Var, Value),
	Term =.. [Var, Value],
	( read(IS, Term) -> true
	; throw(cannot_restore(Var))
	).

% ===========================================================================

:- doc(subsection, "Required Documentation Indices").
% (for a given backend and the current options)

:- use_module(library(aggregates), [findall/3]).

:- export(docst_has_index/2).
docst_has_index(Index, DocSt) :-
        docst_opt(indices(Indices), DocSt),
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
	docst_has_index(IdxName, DocSt).

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
	docst_inputfile(DocSt, S),
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

:- export(get_doc_field/3).
get_doc_field(Id0, Field, loc(S, LB, LE)) :-
%	(Id=title->display('b:'),display(Id),display(':'),display(Field),nl;true),
	( Id0 = pred(Id) -> true ; Id = Id0 ),
	% TODO: accept 'comment' for compatibility, but emit deprecate message
	( clause_read(_, 1, comment(Id, Field), Dict, S, LB, LE)
	; clause_read(_, 1, doc(Id, Field), Dict, S, LB, LE)
	),
	bind_dict_varnames(Dict).

:- export(get_doc_field_dict/3).
get_doc_field_dict(Id0, Field, Dict) :-
	% TODO: accept 'comment' for compatibility, but emit deprecate message
	( Id0 = pred(Id) -> true ; Id = Id0 ),
	( clause_read(_, 1, comment(Id, Field), Dict, _, _, _)
	; clause_read(_, 1, doc(Id, Field), Dict, _, _, _)
	).

:- export(bind_dict_varnames/1).
:- pred bind_dict_varnames(Dict) # "Binds the variables in @var{Dict}
   to the corresponding names (i.e., the names that appeared in the
   source during read.".

bind_dict_varnames([]).
bind_dict_varnames([VarName=Var|Rest]) :-
	VarName=Var,
	bind_dict_varnames(Rest).

% Like get_doc, but emits 'note' errors only if doing mainmod
:- export(get_mod_doc/3).
get_mod_doc(P, DocSt, Value) :-
	( docst_currmod_is_main(DocSt) ->
	    ErrorType = note
	; ErrorType = ignore
	),
	get_doc(P, ErrorType, DocSt, Value).

% ---------------------------------------------------------------------------

% TODO: Like @regtype{filetype/1}, but includes 'application'?
:- export(modtype/1).
:- regtype modtype/1 # "Represents the type of file being documented.".
:- doc(modtype/1, "@includedef{modtype/1}").

% TODO: merge 'documentation' and 'part'? I can use the level to infer this
modtype(part). % (introduction of a part)
modtype(application).
modtype(documentation). % (like a part, not at first level)
modtype(module).
modtype(user).
modtype(include).
modtype(package).

% :- export(detect_modtype/2).
% modtype is cached in the docstate
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
	docst_mvar_get(DocSt, modinfo, modinfo(_, Base)),
	( defines(Base, main, 0, _, _)
	; defines(Base, main, 1, _, _)
	),
	!,
	FileType = application.
%% Else, we need to infer the type
detect_modtype(DocSt, FileType) :-
	docst_mvar_get(DocSt, modinfo, modinfo(M, _)),
	( M = user(_) ->
	    FileType = package % TODO: This is wrong, check for ":- package" declarations instead
	; FileType = module
	).

:- export(docst_modtype/2).
docst_modtype(DocSt, ModType) :-
	( docst_mvar_get(DocSt, modtype, ModType) ->
	    true
	; docst_currmod(DocSt, M), % TODO: Wrong! M is not Base!?
	  docst_gdata_query(DocSt, M, modtype(ModType0)) ->
	    ModType = ModType0
	; % TODO: this could be sometimes wrong
	  % TODO: Copy the modtype in docst_new_sub/3 instead
	  ModType = documentation % for sections in subfiles...
	).

% ---------------------------------------------------------------------------

:- export(get_first_loc_for_pred/3).
% TODO: Need to check for loops <- JFMC: old comment, what does it mean?
get_first_loc_for_pred(F, A, loc(S, L0, L1)) :-
	functor(Head, F, A),
	clause_read(_, Head, _Body, _VarNames, S, L0, L1),
	!.
get_first_loc_for_pred(F, A, loc(S, L0, L1)) :-
	clause_read(_, 1, multifile(F/A), _, S, L0, L1),
	!.
get_first_loc_for_pred(_, _, _).



