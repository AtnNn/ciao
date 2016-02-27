% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "CiaoDE Compilation/Installation module").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(autoconfig)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(distutils(distpkg_generator))).
:- use_module(library(distutils(svn_tools))).
:- use_module(library(distutils(setperms))).
:- use_module(library(distutils), [output_to_file/2]).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
:- use_module(ciaodesrc(makedir('DOCCOMMON'))).
:- use_module(ciaodesrc(makedir(package))).

output_dir_name := ~atom_concat(~component_src(ciaode), '/package').

target_dir(SourceDir, TargetDir) :-
	atom_concat(~component_src(ciaode), '/',        SourceDir),
	atom_concat(SourceDir,              'package/', TargetDir).

do_installer(PackageType, Descs) :-
	target_dir(SourceDir, TargetDir),
	mkdir_perm(TargetDir, ~perms),
	string_to_file("", ~atom_concat(TargetDir, 'NODISTRIBUTE')),
	distpkg_name_version(PackageNameVersion),
	exclude_files(PackageType, ExcludeFiles),
	bold_message("Creating ~w tarball ~w ~w, please be patient ...",
	    [PackageType, PackageNameVersion, Descs]),
	build_distpkg_codeitem_contents(PackageType, SourceDir, TargetDir,
	    PackageNameVersion, ExcludeFiles, FileList),
	list(Descs, build_distpkg_codeitem(SourceDir, TargetDir,
		PackageNameVersion, PackageType, FileList)).

descfile <- :- descfile.
descfile :-
	distpkg_name(PackageName),
	distpkg_name_version(PackageNameVersion),
	distpkg_version(PackageVersion),
	distpkg_version_nice(PackageVersionNice),
	svn_repository(SvnRepository),
	distpkg_revision(PackageRevision),
	svn_revision_time(SvnRepository, PackageRevision, PackageSvnTime),
	%
	target_dir(_, TargetDir),
	mkdir_perm(TargetDir, ~perms),
	DescFile = ~atom_concat(TargetDir, 'desc.tmpl'),
	output_to_file(distpkg_generate_meta(PackageName,
		PackageNameVersion, PackageVersion, PackageVersionNice,
		PackageSvnTime, distpkg_codeitem_kind, distpkg_docitem_kind),
	    DescFile).
