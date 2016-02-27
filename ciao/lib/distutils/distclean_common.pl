:- module(_, _, [make, fsyntax, regexp]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(distutils)).
:- use_module(library(distutils(skip_settings))).
:- use_module(library(system)).
:- use_module(library(make(make_rt))).
:- use_module(library(make(system_extra))).
:- use_module(library(compiler)).

comment(module, "

@section{cleaning routines}

Have in mind the next definitions:

clean     <- remove auto generated temporary and process files.

rawclean  <- clean + remove files which must not be distributed in a
             raw distribution.  A raw distribution is a precompiled
             (almost ready to use) distribution.

").

clean <- [] # "Remove auto generated temporary files." :-
	recursive_clean('./').

rawclean <- [] #
"clean + remove files which must not be distributed in a
    raw distribution.  A raw distribution is a precompiled
    (almost ready to use) distribution."
	:-
	recursive_rawclean('./').

% Specific files that must be deleted in order to clean CiaoDE

recursive_readmes_distclean <- :-
	recursive_readmes_distclean.

recursive_readmes_distclean :-
	find('./', recursive_readmes_file, recursive_readmes_dir('./',
		~skip_dirs, ~nodist_dirs, distclean), dir_after_dummy_).

recursive_readmes_realclean <- :-
	recursive_readmes_realclean.

recursive_readmes_realclean :-
	find('./', recursive_readmes_file, recursive_readmes_dir('./',
		~skip_dirs, ~nodist_dirs, realclean), dir_after_dummy_).

recursive_readmes_file(_BaseDir, _File) :-
	fail.

recursive_readmes_dir(CurrBaseDir, BaseDir, SkipDirs, NDFiles, CleanType,
	    Dir) :-
	is_distributable_dir(CurrBaseDir, BaseDir, SkipDirs, '', NDFiles, Dir),
	working_directory(W, W),
	cd(~atom_concat(CurrBaseDir, Dir)),
	( file_exists('RSETTINGS.pl'), file_exists('SETTINGS.pl') ->
	    (
		display_list(['Cleaning ', CurrBaseDir, Dir, '...\n']),
		-use_module('SETTINGS'),
		( CleanType == realclean ->
		    -- use_module('RSETTINGS'),
		    _:readmetarget(R),
		    (
			_:component(C),
%			display(~atom_concat([R,'/',C])),nl,
			-- delete_file(~atom_concat([R, '/', C])),
			fail
		    ; true
		    ),
		    --unload('RSETTINGS')
		; true
		),
		( _:component(C),
%		    display(~atom_concat(C, '.pl\n')),
		    del_file_nofail(~atom_concat(C, '.pl')),
		    del_file_nofail(~atom_concat(C, '.ascii')),
		    fail
		; true
		),
%		--unload('LPDOCCOMMON.pl'),
		--unload('SETTINGS') ->
		cd(W),
		fail
	    ; true
	    )
	; true
	),
	cd(W).


recursive_skel_ext_distclean :-
	recursive_ext_distclean('.skel', ['']).

recursive_ext_distclean(SourceExt, OutputExts) :-
	find('./', recursive_ext_file(SourceExt, OutputExts,
		['Makefile.pl', 'SETTINGS.pl']), is_distributable_dir('./',
		~skip_dirs, '', ~nodist_dirs), dir_after_dummy_).

recursive_ext_file(CurrBaseDir, SourceExt, OutputExts, SkipFiles, File) :-
	\+(member(File, SkipFiles)),
	atom_concat(FileBase, SourceExt, File),
	(
	    member(OutputExt, OutputExts),
	    atom_concat([CurrBaseDir, '/', FileBase, OutputExt],
		OutputFileName),
	    file_exists(OutputFileName),
	    -file_property(OutputFileName, type(regular)),
	    -del_file_nofail(OutputFileName),
%	    display(OutputFileName),nl,
	    fail
	; true
	),
	!,
	fail.

% ----------------------------------------------------------------------------
recursive_lpdoc_docsclean <- :-
	recursive_lpdoc_docsclean.

recursive_lpdoc_docsclean :-
	find('./', recursive_lpdoc_file, recursive_lpdoc_dir('./',
		~nodist_dirs, ~skip_dirs), dir_after_dummy_).

recursive_lpdoc_file(_BaseDir, _File) :- fail.

recursive_lpdoc_dir(CurrBaseDir, BaseDir, NDFiles, SkipDirs, Dir) :-
	is_distributable_dir(CurrBaseDir, BaseDir, SkipDirs, '', NDFiles, Dir),
	working_directory(W, W),
	cd(~atom_concat(CurrBaseDir, Dir)),
	( file_exists('SETTINGS.pl'),
	    \+ (file_exists('RSETTINGS.pl')) ->
	    (
		display_list(['Cleaning ', CurrBaseDir, Dir, '...\n']),
		-- use_module('SETTINGS.pl'),
		( get_value(doc_structure, DocStr),
		  mainfile_from_doc_structure(DocStr, MainFile) ->
%		    display_list(['docsclean(', MainFile, ')\n'])
		    docsclean(MainFile)
		; true
		),
		-- unload('SETTINGS.pl') ->
		cd(W),
		fail
	    ; true
	    )
	; true
	),
	cd(W).

% mainfile_from_doc_structure(DocStr, MainFile)
mainfile_from_doc_structure(X, Y) :- atom(X), !, Y = X.
mainfile_from_doc_structure(X-_, Y) :- atom(X), !, Y = X.
mainfile_from_doc_structure([X], Y) :- !,
	mainfile_from_doc_structure(X, Y).

% ----------------------------------------------------------------------------
recursive_clean(BaseDir) :-
	recursive_generic_clean(BaseDir, ~skip_dist_files(bin),
	    ~nodist_dirs, ~skip_dirs).

recursive_rawclean(BaseDir) :-
	recursive_generic_clean(BaseDir, ~skip_dist_files(bin),
	    ~nodist_dirs, ~skip_dirs).

recursive_distclean(BaseDir) :-
	recursive_generic_clean(BaseDir, ~skip_dist_files(src),
	    ~nodist_dirs, ~skip_dirs).

recursive_realclean(BaseDir) :-
	recursive_generic_clean(BaseDir, ~skip_dist_files(src),
	    ~nodist_dirs, ~skip_dirs).

recursive_generic_clean(BaseDir, Pattern, NDFiles, SkipDirs) :-
	find(BaseDir, recursive_clean_file(Pattern),
	    is_distributable_dir(BaseDir, SkipDirs, '', NDFiles), dir_after_dummy_).

recursive_clean_file(CurrBaseDir, Pattern, File) :-
	match_pred(Pattern, File),
	atom_concat(CurrBaseDir, File, FileName),
	--del_file_nofail(FileName),
%	display_list([FileName,'\n']),
	!,
% fail to avoid the File be added to the list
	fail.
