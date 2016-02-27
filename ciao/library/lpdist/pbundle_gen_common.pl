:- module(pbundle_gen_common, [], [ciaopaths, assertions, make, fsyntax]).

:- doc(title,  "Common Definitions for Installer Generation").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(aggregates)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system_extra)).
:- use_module(library(file_utils), [output_to_file/2]).

:- use_module(library(lpdist(pbundle_generator))).
:- use_module(library(lpdist(svn_tools))).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(ciao_config_db))).
:- use_module(library(lpdist(ciao_config_options)), [perms/1]).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

% ===========================================================================

% The directory where lpdist files are placed
:- export(lpdist_dir/1).
lpdist_dir := bundle_src(ciao)/'library'/'lpdist'.

% ===========================================================================

% TODO: Generalize this part for any bundle

% TODO: See library/lpdist/pbundle_versions.pl
pbundle_name := ~bundle_packname(~bundle_wholesystem).
pbundle_name_version := ~bundle_packname_version_patch_rev(~bundle_wholesystem).
pbundle_version := ~atom_concat('-', ~bundle_version_patch_rev(~bundle_wholesystem)).
pbundle_version_nice :=
	~atom_concat([~bundle_version_patch(~bundle_wholesystem),
	              ' (r', ~bundle_svn_revatm, ')']).

:- include(ciaosrc(makedir(platdep_modules))).

pbundle_codeitem_kind := tgz|rpm_x86|deb_x86|win|dmg.
% TODO: Extract from the sub-bundles!
pbundle_docitem_kind := 
    docpart("Ciao Manual", ciao, [html, pdf])|
    docpart("CiaoPP Manual", ciaopp, [html, pdf])|
    docpart("LPdoc Manual", lpdoc, [html, pdf]).
% TODO: Missing some internal manuals, add them.

% ===========================================================================

:- export(pbundle_output_dir/1).
% TODO: The definition of directory is repeated in ciaobot/SHARED
%       (PBUNDLE_BUILD_DIR). Share the definition.
pbundle_output_dir := ~fsR(sys_dir(build)/'pbundle').

:- export(create_pbundle_output_dir/0).
:- pred create_pbundle_output_dir # "Make sure that the directory
   where generated pbundle files are placed exists and has the
   NODISTRIBUTE mark.".

create_pbundle_output_dir :-
	TargetDir = ~pbundle_output_dir,
	mkdir_perm(TargetDir, ~perms),
	% TODO: use 'directory mark' preds
	string_to_file("", ~fsR(TargetDir/'NODISTRIBUTE')).

:- export(gen_pbundle__common/2).
gen_pbundle__common(PackageType, Descs) :-
	SourceDir = ~atom_concat(~fsR(bundle_src(ciaode)), '/'),
	create_pbundle_output_dir,
	TargetDir = ~pbundle_output_dir,
	pbundle_name_version(PackageNameVersion),
	exclude_files(PackageType, ExcludeFiles),
	bold_message("Creating ~w tarball ~w ~w, please be patient ...",
	    [PackageType, PackageNameVersion, Descs]),
	TargetDir2 = ~atom_concat(TargetDir, '/'), % TODO: remove '/'
	build_pbundle_codeitem_contents(PackageType, SourceDir, TargetDir2,
	    PackageNameVersion, ExcludeFiles, FileList),
	list(Descs, build_pbundle_codeitem(SourceDir, TargetDir2,
		PackageNameVersion, PackageType, FileList)).

% Note: 'build' directory is not fully excluded (see skip_dist_dirs/2)
% TODO: obtain this list from other place, or write those files in build/config?
exclude_file(_) := 'build/CONFIG_input'. % TODO: necessary?
exclude_file(_) := 'build/CONFIG_mkf'.
exclude_file(_) := 'build/CONFIG_sh'.
exclude_file(_) := 'build/CONFIG_saved'.
exclude_file(noa, File) :-
	platdep_module(_, _, ModulePl),
	atom_concat(Module, '.pl', ModulePl),
	exclude_ext(Ext),
	atom_concat(['ciao/', Module, Ext], File).

% TODO: query the compiler for that
exclude_ext := '.po'|'.itf'|'.asr'|'.ast'.

exclude_files(PackageType) := ~findall(File, exclude_file(PackageType, File)).

% TODO: Used from the compile farm (SHARED file)
gen_pbundle__descfile <- :- gen_pbundle__descfile.

:- export(gen_pbundle__descfile/0).
% Generate the desc.tmpl file (see pbundle_meta)
gen_pbundle__descfile :-
	pbundle_name(PackageName),
	pbundle_name_version(PackageNameVersion),
	pbundle_version(PackageVersion),
	pbundle_version_nice(PackageVersionNice),
	bundle_svn_repository(SvnRepository),
	bundle_svn_revstr(PackageRevision),
	svn_revision_time(SvnRepository, PackageRevision, PackageSvnTime),
	%
	TargetDir = ~pbundle_output_dir,
	mkdir_perm(TargetDir, ~perms),
	DescFile = ~fsR(TargetDir/'desc.tmpl'),
	output_to_file(pbundle_generate_meta(PackageName,
		PackageNameVersion, PackageVersion, PackageVersionNice,
		PackageSvnTime, pbundle_codeitem_kind, pbundle_docitem_kind),
	    DescFile).

