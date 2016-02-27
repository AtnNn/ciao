:- module(autodoc_filesystem, [], [dcg, assertions, regtypes, basicmodes, fsyntax]).

:- doc(title, "Filesystem Abstraction").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides definitions to assign unique
   file-system paths and names for each of the intermediate and final
   results of documentation generation.").

:- use_module(lpdocsrc(src(autodoc_settings))).
:- use_module(lpdocsrc(src(autodoc_structure))).
:- use_module(lpdocsrc(src(autodoc_state)), [backend_id/1]).

:- use_module(library(aggregates)).
:- use_module(library(make(system_extra)), [(-) /1]).
:- use_module(library(terms), [atom_concat/2]).

%% ---------------------------------------------------------------------------

% TODO: Should really use sourcename, but not really supported until we eliminate 
%       the makefile completely. (Check again this to-do -- JFMC)

:- export(filename/1).
:- regtype filename(X) # "@var{X} is the name of a file.".

filename(X) :- atm(X).

% TODO: Check again; we sometimes use basename as module names
:- export(basename/1).
:- regtype basename(X) # "@var{X} is the base name of a file (without extension).".

basename(X) :- atm(X).

% ---------------------------------------------------------------------------

:- export(subtarget/1).
:- regtype subtarget/1 # "The kind of intermediate/final results for a
   single documentation processing unit (module).".

subtarget(fr). % final result
subtarget(fr_aux(_)). % auxiliary files included in the final result
subtarget(fr_alt(_)). % alternative views/versions of the final results (e.g. PDF, PS, infoindex)
subtarget(cr). % intermediate results in the specified format
subtarget(dr). % doctree representation (almost format agnostic)
subtarget(rr). % local references (joint later as global refs)
subtarget(gr). % globally resolved references (including biblio)

% Use the output name for this subtarget?
% TODO: Base on some backend option?
subtarget_uses_output_name(fr, _) :- !.
subtarget_uses_output_name(fr_alt(_), _) :- !.
subtarget_uses_output_name(cr, man) :- !.

% Subtargets that are placed in the output dir (and not in the cache dir)
subtarget_is_final(cr, html) :- !.
subtarget_is_final(cr, man) :- !.
subtarget_is_final(fr_alt(Ext), html) :- !, \+ Ext = 'htmlmeta'.
subtarget_is_final(fr_aux(_), html) :- !.
subtarget_is_final(fr, _) :- !.
subtarget_is_final(fr_alt(_), _) :- !.

:- pred target_suffix/3 : atm * subtarget * atm # "A
   final suffix given a format and subtarget.".
% TODO: put everything in a temporal directory so that suffixes can be mixed?

target_suffix(Backend, Subtarget, Suffix) :-
	( Subtarget = fr -> % final result
	    backend_final_suffix(Backend, Suffix)
	; Subtarget = cr -> % intermediate
	    backend_temp_suffix(Backend, Suffix)
	; Subtarget = dr -> % doctree
	    backend_temp_suffix(Backend, Suffix0),
	    atom_concat(Suffix0, '_dr', Suffix)
	; Subtarget = rr -> % local references
	    backend_temp_suffix(Backend, Suffix0),
	    atom_concat(Suffix0, '_rr', Suffix)
	; Subtarget = gr -> % global references
	    backend_temp_suffix(Backend, Suffix0),
	    atom_concat(Suffix0, '_gr', Suffix)
	).

% TODO: This is the supported format suffix for components
backend_temp_suffix(texinfo,'texic').
backend_temp_suffix(html,'html').
backend_temp_suffix(man,'manl').

backend_final_suffix(texinfo, 'texi').
backend_final_suffix(html, 'htmlmeta').
%backend_final_suffix(man, 'manmeta').

% ---------------------------------------------------------------------------

% TODO: For information purposes, also for cleaning. This could be defined
%   in the backends.
:- export(file_format_name/2).
file_format_name(dvi,   'TeX device-indep').
file_format_name(ps,    'postscript').
file_format_name(pdf,   'Adobe PDF (acrobat)').
file_format_name(texi,  'GNU texinfo source').
%file_format_name('HLP', 'Windows help').
file_format_name(txt,   'plain text').
file_format_name(ascii, 'ASCII plain text').
file_format_name(html,  'HTML hypertext').
file_format_name(htmlmeta,  'HTML hypertext (metafile)').
file_format_name(info,  'GNU info hypertext').
file_format_name(infoindex, 'GNU info hypertext (directory)').
file_format_name(manl,  'UNIX man').

:- export(supported_file_format/1).
supported_file_format(Ext) :-
	file_format_provided_by_backend(Ext, _, _).

% TODO: Store in the backends?
:- export(file_format_provided_by_backend/3).
:- pred file_format_provided_by_backend(Ext, Backend, Subtarget) :: atm * backend_id * atm #
   "@var{Backend} is the backend that generates files with format @var{Ext}".
file_format_provided_by_backend(texi, texinfo, fr).
file_format_provided_by_backend(dvi, texinfo, fr_alt(dvi)).
file_format_provided_by_backend(ps, texinfo, fr_alt(ps)).
file_format_provided_by_backend(pdf, texinfo, fr_alt(pdf)).
file_format_provided_by_backend(info, texinfo, fr_alt(info)).
file_format_provided_by_backend(infoindex, texinfo, fr_alt(infoindex)).
file_format_provided_by_backend(ascii, texinfo, fr_alt(ascii)).
file_format_provided_by_backend(manl, man, cr).
file_format_provided_by_backend(html, html, cr).
file_format_provided_by_backend(htmlmeta, html, fr).

% ---------------------------------------------------------------------------

:- data computed_output_name/2. % name of main output
:- data computed_output_dir/2. % directory for output
:- data computed_cache_dir/2. % directory cached temporal results

:- export(clean_fs_db/0).
:- pred clean_fs_db # "Clean the cached information for the
   filesystem mapping of the documentaton generation.".

clean_fs_db :-
	retractall_fact(computed_output_name(_, _)),
	retractall_fact(computed_output_dir(_, _)),
	retractall_fact(computed_cache_dir(_, _)).

:- export(get_output_dir/2).
:- pred get_output_dir(Backend, Dir) # "Obtain the @var{Dir} directory
where final documentation files will be stored".

get_output_dir(Backend, Dir) :-
	computed_output_dir(Backend, Dir0), !, Dir = Dir0.
get_output_dir(Backend, Dir) :-
	( Backend = html, setting_value(htmldir, Dir0), \+ Dir0 = '' ->
	    path_name(Dir0, Dir1)
	; % TODO: missing some root dir
	  Dir1 = ''
	),
	( output_packed_in_dir(Backend) ->
	    % Use a directory inside 'htmldir'
	    main_output_name(Backend, OutBase),
	    atom_concat([Dir1, OutBase, '.', Backend, '/'], Dir)
	; % Store in 'htmldir' directly
	  Dir = Dir1
	),
	assertz_fact(computed_output_dir(Backend, Dir)).

:- use_module(library(distutils(dirutils)), [path_name/2]).

:- export(get_cache_dir/2).
:- pred get_cache_dir(Backend, Dir) # "Obtain the @var{Dir} directory
where final documentation files will be stored".

get_cache_dir(Backend, Dir) :-
	computed_cache_dir(Backend, Dir0), !, Dir = Dir0.
get_cache_dir(Backend, Dir) :-
	% TODO: missing some root dir
	main_output_name(Backend, OutBase),
	atom_concat([OutBase, '.tmp-', Backend, '/'], Dir),
	assertz_fact(computed_cache_dir(Backend, Dir)).

% ---------------------------------------------------------------------------

% Make sure that the output directory exists
:- export(ensure_output_dir/1).
ensure_output_dir(Backend) :-
	get_output_dir(Backend, Dir),
	( Dir = '' -> true ; make_dirpath(Dir) ).

% Make sure that the cache directory exists
:- export(ensure_cache_dir/1).
ensure_cache_dir(Backend) :-
	get_cache_dir(Backend, Dir),
	( Dir = '' -> true ; make_dirpath(Dir) ).

:- use_module(library(system), [make_dirpath/1]).

% ---------------------------------------------------------------------------
% Some special cases for @pred{absfile_for_subtarget/4}

:- export(main_absfile_in_format/2).
:- pred main_absfile_in_format(Ext, File) # 
   "@var{File} is the absolute file name for the documentation in
   @var{Ext} format of the @em{main} module".

% TODO: note that Ext is not a backend, but a file format. Made that 
% explicit in the documentation.
main_absfile_in_format(Ext, File) :-
	file_format_provided_by_backend(Ext, Backend, Subtarget),
	main_absfile_for_subtarget(Backend, Subtarget, File).

:- export(main_absfile_for_subtarget/3).
main_absfile_for_subtarget(Backend, Subtarget, File) :-
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, Subtarget, File).

:- export(absfile_for_aux/3).
:- pred absfile_for_aux(AuxName, Backend, AbsFile) # "Absolute file
   for an auxiliary output file (e.g. CSS, images, etc.)".

absfile_for_aux(AuxName, Backend, AbsFile) :-
	absfile_for_subtarget(AuxName, Backend, fr_aux(''), AbsFile).

% ---------------------------------------------------------------------------

:- export(absfile_for_subtarget/4).
absfile_for_subtarget(Mod, Backend, Subtarget, File) :-
	subtarget_name(Backend, Subtarget, Mod, File0),
	absfile_for_subtarget_(File0, Backend, Subtarget, File).

% Obtain the name (without extension) for the given subtarget
subtarget_name(Backend, Subtarget, Mod, NameExt) :-
	( get_mainmod(Mod),
	  subtarget_uses_output_name(Subtarget, Backend) ->
	    main_output_name(Backend, Base)
	; Base = Mod
	),
	( Subtarget = fr_aux('') ->
	    NameExt = Base
	; ( Subtarget = fr_aux(Ext) -> true
	  ; Subtarget = fr_alt(Ext) -> true
	  ; target_suffix(Backend, Subtarget, Ext)
	  ),
	  atom_concat([Base, '.', Ext], NameExt)
	).

% The final (absolute) base
% TODO: Output and temporal file handling needs a major rework
% absfile_for_subtarget_(FinalBase, Backend, Subtarget, FinalAbsBase)
absfile_for_subtarget_(Base, Backend, Subtarget, AbsFile) :-
	( subtarget_is_final(Subtarget, Backend) ->
	    get_output_dir(Backend, Dir)
	; get_cache_dir(Backend, Dir)
	),
	concat_dir(Dir, Base, AbsFile).

concat_dir(Dir, FinalBase, FinalAbsBase) :-
	( Dir = '' ->
	    FinalAbsBase = FinalBase
	; atom_concat(Dir, FinalBase, FinalAbsBase)
	).

% ---------------------------------------------------------------------------

:- use_module(library(component_registry), [component_src/2]).
:- use_module(lpdocsrc(src(component_versions))).

% Note: I cannot obtain the version from version_maintenance at this
%       point, since main_output_name needs to be calculated before
%       the mainfile is read.
% TODO: Generate documentation symlinks automatically?
% TODO: Reuse this for binaries in component installation (this code
%       and the links)

:- export(main_output_name/2).
% The output name of the generated manual. The version number will be
% concatenated if available.
% TODO: Make sure that this behaviour and the lpdoc documentation are
%       consistent.
main_output_name(Backend, NV) :-
	computed_output_name(Backend, NV0), !, NV = NV0.
main_output_name(Backend, NV) :-
	( ( setting_value(output_name, OutputBase0) ->
	      OutputBase1 = OutputBase0
	  ; get_mainmod(InBase),
	    modname_nodoc(InBase, OutputBase1)
	  )
	),
	% Include the version (if required)
	( setting_value(parent_component, Component),
	  component_src(Component, Dir),
	  \+ setting_value(doc_mainopts, no_versioned_output) ->
	    % Use the component version for the output name
	    atom_concat(Dir, '/', Dir1),
	    component_obtain_version(Dir1, V),
	    atom_concat([OutputBase1, '-', V], NV)
	; % Do not use the version for the output name
	  NV = OutputBase1
	),
	assertz_fact(computed_output_name(Backend, NV)).

% the physical (on-disk) module name without the '_doc' suffix (if present)
% TODO: Do something similar for packages whose documentation is stored in 
%       _doc files?
modname_nodoc(Base0, Base) :-
	( atom_concat(Base1, '_doc', Base0) -> Base = Base1
	; Base = Base0
	).

% ---------------------------------------------------------------------------

:- export(get_subbase/3).
:- pred get_subbase(Base, Sub, SubBase) => basename * atm * basename
   # "@var{SubBase} is the name for the sub-file (@var{Sub})
     associated with @var{Base}".
% e.g., 'ciaointro' for the introduction section of 'ciao', when we
% want it to be a separate file.

get_subbase(Base, Sub, SubBase) :-
	atom_concat(Base, Sub, SubBase).

% ---------------------------------------------------------------------------

:- export(absfile_to_relfile/3).
:- pred absfile_to_relfile(A, Backend, B) # "Obtain the relative path,
   w.r.t. the output directory, of an absolute file. This is useful,
   e.g., for URLs.".

absfile_to_relfile(A, Backend, B) :-
	get_output_dir(Backend, Dir),
	( Dir = '' ->
	    B = A
	; atom_concat(Dir, B0, A) ->
	    B = B0
	; B = A
	).

% ---------------------------------------------------------------------------
% Cleaning final and temporary files

:- use_module(library(distutils(dirutils)), [delete_files_and_dirs/1]).
:- use_module(library(make(system_extra)),
	    [ % Local to system_extra...
		(-) /1,
		ls/2,
		del_file_nofail/1,
		delete_files/1,
		cat/2,
		del_endings_nofail/2
	    ]).

:- export(clean_all/0).
clean_all :-
	clean_intermediate,
	clean_temp_no_texi,
	clean_texi.

:- export(clean_docs_no_texi/0).
clean_docs_no_texi :-
	clean_intermediate,
	clean_temp_no_texi.

:- export(clean_all_temporal/0).
clean_all_temporal :-
	clean_intermediate,
	clean_texi.

:- export(clean_intermediate/0).
clean_intermediate :-
	clean_tex_intermediate,
	clean_other_intermediate.

clean_temp_no_texi :-
	findall(WExt, (file_format_name(Ext, _), \+ Ext = 'texi', atom_concat('|*.', Ext, WExt)),
	    WExts),
	-delete_files_and_dirs(~ls(~atom_concat(WExts))).

clean_texi :-
	delete_files(~ls('*.texi')).

clean_other_intermediate :-
	% TODO: It should not delete autofig*.png files, right? (indeed they are not generated here)
	% TODO: Use a directory for temporary files?
	-delete_files(~ls('*~|*.itf|*.po|*.asr|*.infoindex|*.err|*.tmp|*.log|*.aux|*.blg|*.bbl|autofig*.jpg|autofig*.png|autofig*.eps|*.htmlmeta')),
	( clean_suffix(Suffix),
	  atom_concat('*.', Suffix, Pattern),
	  -delete_files_and_dirs(~ls(Pattern)),
	  fail
	; true
	).

% TODO: Generalize (use backends)
clean_suffix('tmp-html').
clean_suffix('tmp-man').
clean_suffix('tmp-texinfo').
% clean_suffix('txt_dr').
% clean_suffix('manl_rr').
% clean_suffix('manl_gr').
% clean_suffix('manl_dr').
% clean_suffix('html_rr').
% clean_suffix('html_gr').
% clean_suffix('html_dr').
% clean_suffix('texic').
% clean_suffix('texic_rr').
% clean_suffix('texic_gr').
% clean_suffix('texic_dr').

:- export(clean_tex_intermediate/0).
% Clean the temporal files created by TeX
clean_tex_intermediate :-
	( % Trick to obtain the base for .texi file
	  main_absfile_in_format('texi', File),
	  atom_concat(OutputBase, '.texi', File) ->
	    del_endings_nofail(
		['.aux', '.cp', '.cps', '.fn', '.fns', '.ky', '.kys', '.log',
		    '.tp', '.tps', '.op', '.ops', '.fi', '.fis', '.pd', '.pds',
		    '.pr', '.prs', '.ug', '.ugs', '.co', '.cos', '.fu', '.fus',
		    '.li', '.lis', '.pg', '.pgs', '.ap', '.aps', '.mo', '.mos',
                    '.au', '.aus', '.gl', '.gls', '.te', '.tes', '.vr', '.vrs',
		    '.de', '.des', '.toc', '.bbl', '.blg'
		],
		OutputBase)
	; true
	),
	( setting_value(libtexinfo, no) -> true
	; del_file_nofail('texinfo.tex')
	).

% ---------------------------------------------------------------------------

% The output (for the given format) is packed in a directory
% TODO: Move as options?
output_packed_in_dir(Backend) :-
	% Only HTML, when not generating a website
	Backend = html,
	\+ setting_value(html_layout, 'website_layout').

