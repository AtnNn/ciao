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
:- use_module(library(lpdist(ciao_bundle_db))).

% ---------------------------------------------------------------------------

% TODO: Add as external options
pbundle_codeitem_kind := tgz|rpm_x86|deb_x86|win|dmg.
% TODO: Simplify, extract from sub-bundles, etc.
% TODO: Missing some internal manuals, add them.
pbundle_docitem_kind := manual_html|manual_pdf.
% Sub-bundles whose documentation is distributed explicitly as a docitem
% TODO: refine
% TODO: Not here
bundle_doc_subbundles(ciao, ciaosys, ciao, "Ciao Manual").
bundle_doc_subbundles(ciao, ciaopp, ciaopp, "CiaoPP Manual").
bundle_doc_subbundles(ciao, lpdoc, lpdoc, "LPdoc Manual").

:- use_module(library(file_utils), [output_to_file/2]).

:- export(pbundle_generate_meta/2).
% Generate the metadata file which contains all the produced output of
% the bundle build process.
pbundle_generate_meta(Bundle, DescFile) :-
	pbundle_generate_meta_(Bundle, Desc),
	clauses_to_file(Desc, DescFile).

:- export(pbundle_generate_meta_/2).
pbundle_generate_meta_(Bundle, Desc) :-
	findall(F, enum_pbundle_code_items(Bundle, F), Fs),
	findall(D, enum_pbundle_doc_items(Bundle, D), Ds),
	Desc = [% Bundle information (from manifest or source)
                name = ~bundle_name(Bundle),
                packname = ~bundle_packname(Bundle),
		version = ~bundle_version(Bundle),
		patch = ~bundle_patch(Bundle),
		% Commit information
		commit_branch = ~bundle_commit_info(Bundle, branch),
		commit_id = ~bundle_commit_info(Bundle, id),
		commit_date = ~bundle_commit_info(Bundle, date),
		commit_desc = ~bundle_commit_info(Bundle, desc),
		% Items in this packaged bundle (after bundle build)
		docs = Ds,
		code = Fs].

% Enumerate the pbundle items for code files
% TODO: See ciaobot_internals.sh:get_produced_pbundle_item_base
enum_pbundle_code_items(Bundle, Item) :-
	pbundle_codeitem_kind(CodeKind),
	pbundle_code_item(Bundle, CodeKind, Item).

% Enumerate the pbundle items for documentation
enum_pbundle_doc_items(Bundle, Item) :-
	bundle_doc_subbundles(Bundle, SubBundle, SubBundleSuffix, SubBundleTitle),
	pbundle_docitem_kind(PDocKind),
	pbundle_doc_item(Bundle, SubBundle, SubBundleSuffix, SubBundleTitle, PDocKind, Item).

% ---------------------------------------------------------------------------

% Obtain the pbundle_item description for the specified CodeKind
pbundle_code_item(Bundle, CodeKind, Item) :-
	pbundle_codeitem_kind_info(CodeKind, Ext0),
	atom_concat('.', Ext1, Ext0),
	Packname = ~bundle_packname(Bundle),
	Desc = ~bundle_commit_info(Bundle, desc),
	( CodeKind = rpm_x86 ->
	    RPMDesc = ~fix_commit_desc_for_rpm(Desc),
	    PFile = ~atom_concat([Packname, '-', RPMDesc, '.', Ext1]),
	    PFileKind = i386_rpm
	; CodeKind = deb_x86 ->
	    RPMDesc = ~fix_commit_desc_for_rpm(Desc), % TODO: Use RPM scheme too?
	    Packname2 = ~loweratom(Packname),
	    PFile = ~atom_concat([Packname2, '_', RPMDesc, '_', Ext1]),
	    PFileKind = i386_deb
	; PFile = ~atom_concat([Packname, '-', Desc, '.', Ext1]),
	  ( pbundle_file_kind_ext(PFileKind, Ext),
	    atom_concat([_, '.', Ext], PFile) ->
	      true
	  ; fail
	  )
	),
	Item = pbundle_item(PFileKind, "", PFile).

% TODO: Merge with code from ciaobot_internals.sh
% Take a commit desc TAG-N-HASH and generate TAG-N.HASH, where N.HASH
% will be the release number for RPM (no '-' is allowed there).
fix_commit_desc_for_rpm(Desc) := RPMDesc :-
	append(Tag, "-"||NHash, Desc), append(N, "-"||Hash, NHash),
	!,
	append(N, "."||Hash, NHash2),
	append(Tag, "-"||NHash2, RPMDesc).
fix_commit_desc_for_rpm(Desc) := Desc.

% TODO: share implementation
loweratom(X0, X) :- atom_codes(X0, Cs0), lowercodes(Cs0, Cs), atom_codes(X, Cs).

lowercodes([], []).
lowercodes([X0|Xs0], [X|Xs]) :- lowercode(X0, X), lowercodes(Xs0, Xs).

lowercode(X0, X) :- X0 >= 0'A, X0 =< 0'Z, !, X is X0 - 0'A + 0'a.
lowercode(X, X).

% Obtain the pbundle_item description for the specified PDocKind and sub-bundle
pbundle_doc_item(Bundle, _SubBundle, SubBundleSuffix, SubBundleTitle, PDocKind, Item) :-
	pbundle_doc_kind_ext(PDocKind, Ext),
	VersionedPackName = ~bundle_versioned_packname(Bundle),
	atom_concat([VersionedPackName, '_', SubBundleSuffix, '.', Ext], PDoc),
	%
	Item = pbundle_item(PDocKind, SubBundleTitle, PDoc).

% ---------------------------------------------------------------------------

pbundle_doc_kind_ext(manual_pdf, 'pdf').
pbundle_doc_kind_ext(manual_html, 'html').

:- export(pbundle_file_kind_ext/2).
pbundle_file_kind_ext(tar_gz, 'tar.gz').
pbundle_file_kind_ext(i386_rpm, 'i386.rpm').
pbundle_file_kind_ext(i386_deb, 'i386.deb').
pbundle_file_kind_ext(windows, 'exe').
pbundle_file_kind_ext(macosx, 'dmg').

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
% Write clauses to a file

clauses_to_file(Desc, DescFile) :-
	output_to_file(clauses_to_file_(Desc), DescFile).

clauses_to_file_(Desc) :-
	push_prolog_flag(write_strings, on),
	portray_clauses(Desc),
	pop_prolog_flag(write_strings).

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
build_pbundle_codeitem(Name, SourceDir, TargetDir, VersionedPackName,
	    PBundleType, FileList) :-
	pbundle_codeitem_kind_info(Name, PBundleExtension),
	build_package(PBundleExtension, VersionedPackName,
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

build_package(PBundleExtension, VersionedPackName,
	    PBundleTypeSuffix, SourceDir, TargetDir, FileList) :-
	atom_concat([TargetDir, VersionedPackName, PBundleTypeSuffix,
		PBundleExtension], CompressedPBundle),
	atom_concat(CompressedPBundle, '.tmp', TmpCompressedPBundle),
	% TODO: Check that bugs/Pending/exec/exec.pl does not affect this code
	compress_command(PBundleExtension, SourceDir, FileList,
	    TmpCompressedPBundle, Command),
	(
	    atom_concat([_, VersionedPackName, '/'], SourceDir) ->
	    % Link not required:
	    do(Command, nofail)
	;
	    % Link required:
	    atom_concat([SourceDir, '../', VersionedPackName],
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
	    VersionedPackName, ExcludeFileList, FileList) :-
	atom_concat([TargetDir, VersionedPackName,
		~pbundle_codeitem_type_suffix(PBundleType), '.list'], FileList),
	atom_concat(VersionedPackName, '/', BaseDir),
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

