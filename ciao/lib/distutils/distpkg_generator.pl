:- module(distpkg_generator, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- doc(title, "Generation of distpkg"). 

:- doc(module, "This module defines the generation of distribution
   packages (@index{distpkg})").

% (imported from: makedir/makedir_common.pl)

:- use_module(library(autoconfig), []).
:- use_module(library(aggregates)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(distutils(dirutils)), [split_path_and_name/3, get_cwd/1]).
:- use_module(library(make(make_rt))).
:- use_module(library(make(system_extra))).
:- use_module(library(distutils)).
:- use_module(library(distutils(skip_settings))).

% ---------------------------------------------------------------------------

:- export(distpkg_generate_meta/7).
:- meta_predicate distpkg_generate_meta(?, ?, ?, ?, ?, pred(1), pred(1)).
distpkg_generate_meta(PackageName, PackageNameVersion,
	    PackageVersion, PackageVersionNice, Time, PkgFormat, DocFormat) :-
	findall(F, enum_distpkg_code_items(PackageNameVersion, PkgFormat, F), Fs),
	findall(D, enum_distpkg_doc_items(PackageNameVersion, DocFormat, D), Ds),
	Desc = [distpkg_name = PackageName,
	        distpkg_name_version = PackageNameVersion,
		distpkg_version = PackageVersion,
		distpkg_version_nice = PackageVersionNice,
		distpkg_date = Time,
		docs = Ds,
		code = Fs],
	%
	push_prolog_flag(write_strings, on),
	portray_clauses(Desc),
	pop_prolog_flag(write_strings).

:- meta_predicate enum_distpkg_code_items(?, pred(1), ?).
% Enumerate the distpkg items for code files
enum_distpkg_code_items(PackageNameVersion, PkgFormat, Item) :-
	PkgFormat(P),
	distpkg_codeitem_kind_info(P, Ext0),
	atom_concat([PackageNameVersion, Ext0], PkgFile),
	( distpkg_file_kind_ext(PkgFileKind, Ext),
	  atom_concat([_, '.', Ext], PkgFile) ->
	    true
	; fail
	),
	Item = distpkg_item(PkgFileKind, "", PkgFile).

distpkg_file_kind_ext(tar_gz, 'tar.gz').
distpkg_file_kind_ext(i386_rpm, 'i386.rpm').
distpkg_file_kind_ext(i386_deb, 'i386.deb').
distpkg_file_kind_ext(windows, 'exe').
distpkg_file_kind_ext(macosx, 'dmg').

:- meta_predicate enum_distpkg_doc_items(?, pred(1), ?).
% Enumerate the distpkg items for documentation
enum_distpkg_doc_items(PackageNameVersion, DocFormat, Item) :-
	DocFormat(P),
	P = docpart(PkgDocTitle, Pkg, Exts),
	member(Ext, Exts),
	atom_concat([PackageNameVersion, '_', Pkg, '.', Ext], PkgDoc),
	atom_concat('manual_', Ext, PkgDocKind),
	%
	Item = distpkg_item(PkgDocKind, PkgDocTitle, PkgDoc).

:- pred distpkg_codeitem_kind_info(Src, Ext) => atm * atm
   # "@var{Ext} is the extention of @var{Src}. @var{Src} is an allowed
      value of variable @tt{distpkg_codeitem_kind/1}".

distpkg_codeitem_kind_info(zip,     '.zip').
distpkg_codeitem_kind_info(gz,      '.gz').
distpkg_codeitem_kind_info(bz2,     '.bz2').
distpkg_codeitem_kind_info(tgz,     '.tar.gz').
distpkg_codeitem_kind_info(tbz,     '.tar.bz2').
distpkg_codeitem_kind_info(win,     '.exe').
distpkg_codeitem_kind_info(rpm_x86, '.i386.rpm').
distpkg_codeitem_kind_info(deb_x86, '.i386.deb').
distpkg_codeitem_kind_info(dmg,     '.dmg').

% ---------------------------------------------------------------------------

portray_clauses([]).
portray_clauses([X|Xs]) :-
	portray_clause(X),
	portray_clauses(Xs).

% ---------------------------------------------------------------------------

distpkg_codeitem_type_suffix(src, '').
distpkg_codeitem_type_suffix(noa, '-noarch').
distpkg_codeitem_type_suffix(bin) := ~atom_concat(['-bin-', ~get_os, ~get_arch]).
distpkg_codeitem_type_suffix(raw) := ~atom_concat(['-raw-', ~get_os, ~get_arch]).

% ---------------------------------------------------------------------------

:- export(build_distpkg_codeitem/6).
build_distpkg_codeitem(Name, SourceDir, TargetDir, PackageNameVersion,
	    PackageType, FileList) :-
	distpkg_codeitem_kind_info(Name, PackageExtension),
	build_package(PackageExtension, PackageNameVersion,
	    ~distpkg_codeitem_type_suffix(PackageType), SourceDir, TargetDir,
	    FileList).

map_extension('.tar.gz',  'gzip').
map_extension('.tgz',     'gzip').
map_extension('.tar.bz2', 'bzip2').
map_extension('.tbz',     'bzip2').

compress_command(E, SourceDir, FileList, CompressedPackage,
	    Command) :-
	map_extension(E, C),
	!,
	compress_command_tar(C, SourceDir, FileList, CompressedPackage,
	    Command).
compress_command('.zip', SourceDir, FileList, CompressedPackage,
	    Command) :-
	Command = ['( cd ', SourceDir, '.. && zip -@ - -q ) < ', FileList,
	    ' > ', CompressedPackage].

tar(gtar) :- get_os('Solaris'),	!.
tar(tar).

compress_command_tar(CompressApp, SourceDir, FileList,
	    CompressedPackage, Command) :-
	list(FileList),
	!,
	prepare_files(FileList, FileListSpc),
	flatten([~tar, ' --directory ', SourceDir,
		' -cf - --owner=0 --group=0 ', FileListSpc, ' | ',
		CompressApp, ' --best -c > ', CompressedPackage],
	    AppCommand),
	atom_concat(AppCommand, Command).
compress_command_tar(CompressApp, SourceDir, FileList,
	    CompressedPackage, [~tar, ' --directory ', SourceDir, '..',
		' -cf - --owner=0 --group=0 --files-from=', FileList,
		' | ', CompressApp, ' --best -c > ', CompressedPackage]).

prepare_files([],     [' ']).
prepare_files([F|Fs], [A, ' '|As]) :-
	split_path_and_name(F, _, A),
	prepare_files(Fs, As).

build_package(PackageExtension, PackageNameVersion,
	    PackageTypeSuffix, SourceDir, TargetDir, FileList) :-
	atom_concat([TargetDir, PackageNameVersion, PackageTypeSuffix,
		PackageExtension], CompressedPackage),
	atom_concat(CompressedPackage, '.tmp', TmpCompressedPackage),
	compress_command(PackageExtension, SourceDir, FileList,
	    TmpCompressedPackage, Command),
	(
	    atom_concat([_, PackageNameVersion, '/'], SourceDir) ->
	    % Link not required:
	    do(Command, nofail)
	;
	    % Link required:
	    atom_concat([SourceDir, '../', PackageNameVersion],
		SourceDirNameVersion),
	    del_file_nofail(SourceDirNameVersion),
	    copy_file(SourceDir, SourceDirNameVersion, [overwrite, symlink]),
	    do(Command, nofail),
	    del_file_nofail(SourceDirNameVersion)
	),
	del_file_nofail(CompressedPackage),
	rename_file(TmpCompressedPackage, CompressedPackage).

:- export(build_distpkg_codeitem_contents/6).
% Write the contents of a 'codeitem' of a 'distribution package' (which is a set of files)
build_distpkg_codeitem_contents(PackageType, SourceDir, TargetDir,
	    PackageNameVersion, ExcludeFileList, FileList) :-
	atom_concat([TargetDir, PackageNameVersion,
		~distpkg_codeitem_type_suffix(PackageType), '.list'], FileList),
	atom_concat(PackageNameVersion, '/', BaseDir),
	%
	open_output(FileList, Output),
	display_file_list(PackageType, SourceDir, BaseDir, ExcludeFileList),
	close_output(Output).

display_file_list(PackageType, SourceDir, BaseDir, ExcludeFileList) :-
	( % (failure-driven loop)
	  skip_dist_dirs(PackageType, SkipSubDir),
	  enum_distpkg_codeitem_contents(PackageType, SourceDir, SkipSubDir, _, _, BaseFileName),
	    atom_concat(SourceDir, DestFileName, BaseFileName),
	    \+ member(DestFileName, ExcludeFileList),
	    display(BaseDir),
	    display(DestFileName),
	    nl,
	    fail
	; true
	).

:- use_module(library(messages)).

