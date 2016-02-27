:- module(pbundle_generator, [], [ciaopaths,
	assertions, basicmodes, nativeprops, regtypes, fsyntax, hiord]).

:- doc(title, "Generation of Packaged Bundles"). 

:- doc(module, "A @concept{bundle} is the term used in Ciao for a
   collection of related modules (@emph{software package} or
   @emph{component}, do not confuse with @index{packages} as
   libraries). Bundles can be distributed and installed separatelly
   (and they may depend on other bundles).

   This module provides a mechanism for the generation of
   @concept{packaged bundles} for different platforms and operating
   systems, as packaged bundles (@concept{pbundle}).").

% (imported from: makedir/pbundle_gen_common.pl)

:- use_module(library(aggregates)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(dirutils), [split_path_and_name/3]).
:- use_module(library(make(make_rt))).
:- use_module(library(system_extra)).
:- use_module(library(lpdist(distutils)), [enum_pbundle_codeitem_contents/6]).
:- use_module(library(lpdist(skip_settings))).

% ---------------------------------------------------------------------------

:- export(pbundle_generate_meta/7).
:- meta_predicate pbundle_generate_meta(?, ?, ?, ?, ?, pred(1), pred(1)).
pbundle_generate_meta(PBundleName, PBundleNameVersion,
	    PBundleVersion, PBundleVersionNice, Time, PFormat, DocFormat) :-
	findall(F, enum_pbundle_code_items(PBundleNameVersion, PFormat, F), Fs),
	findall(D, enum_pbundle_doc_items(PBundleNameVersion, DocFormat, D), Ds),
	Desc = [pbundle_name = PBundleName,
	        pbundle_name_version = PBundleNameVersion, % TODO: avoid?
		pbundle_version = PBundleVersion,
		pbundle_version_nice = PBundleVersionNice, % TODO: necessary?
		pbundle_date = Time, % TODO: rename by 'build_date' or component_date?
		docs = Ds,
		code = Fs],
	%
	push_prolog_flag(write_strings, on),
	portray_clauses(Desc),
	pop_prolog_flag(write_strings).

:- meta_predicate enum_pbundle_code_items(?, pred(1), ?).
% Enumerate the pbundle items for code files
enum_pbundle_code_items(PBundleNameVersion, PFormat, Item) :-
	PFormat(X),
	pbundle_codeitem_kind_info(X, Ext0),
	atom_concat([PBundleNameVersion, Ext0], PFile),
	( pbundle_file_kind_ext(PFileKind, Ext),
	  atom_concat([_, '.', Ext], PFile) ->
	    true
	; fail
	),
	Item = pbundle_item(PFileKind, "", PFile).

:- export(pbundle_file_kind_ext/2).
pbundle_file_kind_ext(tar_gz, 'tar.gz').
pbundle_file_kind_ext(i386_rpm, 'i386.rpm').
pbundle_file_kind_ext(i386_deb, 'i386.deb').
pbundle_file_kind_ext(windows, 'exe').
pbundle_file_kind_ext(macosx, 'dmg').

:- meta_predicate enum_pbundle_doc_items(?, pred(1), ?).
% Enumerate the pbundle items for documentation
enum_pbundle_doc_items(PBundleNameVersion, DocFormat, Item) :-
	DocFormat(X),
	X = docpart(PDocTitle, Name, Exts),
	member(Ext, Exts),
	atom_concat([PBundleNameVersion, '_', Name, '.', Ext], PDoc),
	atom_concat('manual_', Ext, PDocKind),
	%
	Item = pbundle_item(PDocKind, PDocTitle, PDoc).

:- pred pbundle_codeitem_kind_info(Src, Ext) => atm * atm
   # "@var{Ext} is the extention of @var{Src}. @var{Src} is an allowed
      value of variable @tt{pbundle_codeitem_kind/1}".

pbundle_codeitem_kind_info(zip,     '.zip').
pbundle_codeitem_kind_info(gz,      '.gz').
pbundle_codeitem_kind_info(bz2,     '.bz2').
pbundle_codeitem_kind_info(tgz,     '.tar.gz').
pbundle_codeitem_kind_info(tbz,     '.tar.bz2').
pbundle_codeitem_kind_info(win,     '.exe').
pbundle_codeitem_kind_info(rpm_x86, '.i386.rpm').
pbundle_codeitem_kind_info(deb_x86, '.i386.deb').
pbundle_codeitem_kind_info(dmg,     '.dmg').

% ---------------------------------------------------------------------------

portray_clauses([]).
portray_clauses([X|Xs]) :-
	portray_clause(X),
	portray_clauses(Xs).

% ---------------------------------------------------------------------------

% TODO: This predicate is not used, but it should (at least in assertions)
:- export(pbundle_codeitem_type/1).
:- regtype pbundle_codeitem_type/1 # "The types of files that contains
code in a @index{packaged bundle}.".
pbundle_codeitem_type(src). % Source files
pbundle_codeitem_type(noa). % Platform independent binary files: Not Architecture
pbundle_codeitem_type(bin). % Binary files, including platform dependent files
pbundle_codeitem_type(raw). % Almost all files, minimal number of ignored files

% ---------------------------------------------------------------------------

pbundle_codeitem_type_suffix(src, '').
pbundle_codeitem_type_suffix(noa, '-noarch').
pbundle_codeitem_type_suffix(bin) := ~atom_concat(['-bin-', ~get_os, ~get_arch]).
pbundle_codeitem_type_suffix(raw) := ~atom_concat(['-raw-', ~get_os, ~get_arch]).

% ---------------------------------------------------------------------------

:- export(build_pbundle_codeitem/6).
build_pbundle_codeitem(Name, SourceDir, TargetDir, PBundleNameVersion,
	    PBundleType, FileList) :-
	pbundle_codeitem_kind_info(Name, PBundleExtension),
	build_package(PBundleExtension, PBundleNameVersion,
	    ~pbundle_codeitem_type_suffix(PBundleType), SourceDir, TargetDir,
	    FileList).

map_extension('.tar.gz',  'gzip').
map_extension('.tgz',     'gzip').
map_extension('.tar.bz2', 'bzip2').
map_extension('.tbz',     'bzip2').

compress_command(E, SourceDir, FileList, CompressedPBundle,
	    Command) :-
	map_extension(E, C),
	!,
	compress_command_tar(C, SourceDir, FileList, CompressedPBundle,
	    Command).
compress_command('.zip', SourceDir, FileList, CompressedPBundle,
	    Command) :-
	Command = ['( cd ', SourceDir, '.. && zip -@ - -q ) < ', FileList,
	    ' > ', CompressedPBundle].

tar(gtar) :- get_os('Solaris'), !.
tar(gnutar) :- get_os('DARWIN'), !.
tar(tar).

compress_command_tar(CompressApp, SourceDir, FileList,
	    CompressedPBundle, Command) :-
	list(FileList),
	!,
	prepare_files(FileList, FileListSpc),
	flatten([~tar, ' --directory ', SourceDir,
		' -cf - --owner=0 --group=0 ', FileListSpc, ' | ',
		CompressApp, ' --best -c > ', CompressedPBundle],
	    AppCommand),
	atom_concat(AppCommand, Command).
compress_command_tar(CompressApp, SourceDir, FileList,
	    CompressedPBundle, [~tar, ' --directory ', SourceDir, '..',
		' -cf - --owner=0 --group=0 --files-from=', FileList,
		' | ', CompressApp, ' --best -c > ', CompressedPBundle]).

prepare_files([],     [' ']).
prepare_files([F|Fs], [A, ' '|As]) :-
	split_path_and_name(F, _, A),
	prepare_files(Fs, As).

build_package(PBundleExtension, PBundleNameVersion,
	    PBundleTypeSuffix, SourceDir, TargetDir, FileList) :-
	atom_concat([TargetDir, PBundleNameVersion, PBundleTypeSuffix,
		PBundleExtension], CompressedPBundle),
	atom_concat(CompressedPBundle, '.tmp', TmpCompressedPBundle),
	% TODO: Check that bugs/Pending/exec/exec.pl does not affect this code
	compress_command(PBundleExtension, SourceDir, FileList,
	    TmpCompressedPBundle, Command),
	(
	    atom_concat([_, PBundleNameVersion, '/'], SourceDir) ->
	    % Link not required:
	    do(Command, nofail)
	;
	    % Link required:
	    atom_concat([SourceDir, '../', PBundleNameVersion],
		SourceDirNameVersion),
	    del_file_nofail(SourceDirNameVersion),
	    copy_file(SourceDir, SourceDirNameVersion, [overwrite, symlink]),
	    do(Command, nofail),
	    del_file_nofail(SourceDirNameVersion)
	),
	del_file_nofail(CompressedPBundle),
	rename_file(TmpCompressedPBundle, CompressedPBundle).

:- export(build_pbundle_codeitem_contents/6).
% Write the contents of a 'codeitem' of a 'packaged bundle' (which is a set of files)
build_pbundle_codeitem_contents(PBundleType, SourceDir, TargetDir,
	    PBundleNameVersion, ExcludeFileList, FileList) :-
	atom_concat([TargetDir, PBundleNameVersion,
		~pbundle_codeitem_type_suffix(PBundleType), '.list'], FileList),
	atom_concat(PBundleNameVersion, '/', BaseDir),
	%
	open_output(FileList, Output),
	display_file_list(PBundleType, SourceDir, BaseDir, ExcludeFileList),
	close_output(Output).

display_file_list(PBundleType, SourceDir, BaseDir, ExcludeFileList) :-
	% TODO: either skip_dist_dirs/2 is a parameter or ExcludeFileList is not
	( % (failure-driven loop)
	  skip_dist_dirs(PBundleType, SkipSubDir),
	  enum_pbundle_codeitem_contents(PBundleType, SourceDir, SkipSubDir, _, _, BaseFileName),
	    atom_concat(SourceDir, DestFileName, BaseFileName),
	    \+ member(DestFileName, ExcludeFileList),
	    display(BaseDir),
	    display(DestFileName),
	    nl,
	    fail
	; true
	).

:- use_module(library(messages)).

