:- module(pbundle_gen_common, [], [ciaopaths, assertions, make, fsyntax]).

:- doc(title,  "Common Definitions for Installer Generation").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(aggregates)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system_extra)).

:- use_module(library(lpdist(pbundle_generator))).
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

:- include(ciaosrc(makedir(platdep_modules))).

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
gen_pbundle__common(PackType, Descs) :-
	Bundle = ~bundle_wholesystem,
	SourceDir = ~atom_concat(~fsR(bundle_src(Bundle)), '/'),
	create_pbundle_output_dir,
	TargetDir = ~pbundle_output_dir,
	VersionedPackName = ~bundle_versioned_packname(Bundle),
	exclude_files(PackType, ExcludeFiles),
	bold_message("Creating ~w tarball for ~w ~w, please be patient ...",
	    [PackType, VersionedPackName, Descs]),
	TargetDir2 = ~atom_concat(TargetDir, '/'), % TODO: remove '/'
	build_pbundle_codeitem_contents(PackType, SourceDir, TargetDir2,
	    VersionedPackName, ExcludeFiles, FileList),
	list(Descs, build_pbundle_codeitem(SourceDir, TargetDir2,
		VersionedPackName, PackType, FileList)).

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

exclude_files(PackType) := ~findall(File, exclude_file(PackType, File)).

% TODO: Used from the compile farm (SHARED file)
gen_pbundle__descfile <- :- gen_pbundle__descfile.

:- export(gen_pbundle__descfile/0).
% Generate the desc.tmpl file (see pbundle_meta)
gen_pbundle__descfile :-
	Bundle = ~bundle_wholesystem,
	TargetDir = ~pbundle_output_dir,
	mkdir_perm(TargetDir, ~perms),
	DescFile = ~fsR(TargetDir/'desc.tmpl'),
	pbundle_generate_meta(Bundle, DescFile).

