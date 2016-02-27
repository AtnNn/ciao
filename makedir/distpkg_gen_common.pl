:- module(distpkg_gen_common, [], [ciaopaths, assertions, make, fsyntax]).

:- doc(title,  "Common Definitions for Installer Generation").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(component_registry), [component_src/2]).
:- use_module(library(aggregates)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(distutils(distpkg_generator))).
:- use_module(library(distutils(svn_tools))).
:- use_module(library(make(system_extra))).
:- use_module(library(file_utils), [output_to_file/2]).
:- use_module(ciaodesrc(makedir(makedir_component))).
:- use_module(ciaodesrc(makedir('DOCCOMMON'))).

% ===========================================================================

% TODO: Make this part user-configurable (for other distpkg)

% TODO: See distutils/distpkg_versions.pl
distpkg_name := ~component_packname(~component_wholesystem).
distpkg_name_version := ~component_packname_version_patch_rev(~component_wholesystem).
distpkg_version := ~atom_concat('-', ~component_version_patch_rev(~component_wholesystem)).
distpkg_version_nice :=
	~atom_concat([~component_version_patch(~component_wholesystem),
	              ' (r', ~svn_revision_atom, ')']).

:- include(ciaosrc(makedir(platdep_modules))).

exclude_file(_) := 'ciao/SETTINGS'.
exclude_file(_) := 'ciao/SETTINGS_AUTO'.
exclude_file(noa, File) :-
	platdep_module(_, _, ModulePl),
	atom_concat(Module, '.pl', ModulePl),
	exclude_ext(Ext),
	atom_concat(['ciao/', Module, Ext], File).

exclude_ext := '.po'|'.itf'|'.asr'|'.ast'.

exclude_files(PackageType) := ~findall(File, exclude_file(PackageType, File)).

distpkg_codeitem_kind := tgz|rpm_x86|deb_x86|win|dmg.
distpkg_docitem_kind := 
    docpart("Ciao Manual", ciao, [html, pdf])|
    docpart("CiaoPP Manual", ciaopp, [html, pdf])|
    docpart("LPdoc Manual", lpdoc, [html, pdf]).
% TODO: Missing some internal manuals, add them.

% ===========================================================================

% TODO: Move to /build/package?
:- export(distpkg_output_dir/1).
distpkg_output_dir := ~atom_concat(~component_src(ciaode), '/package').

:- export(do_installer/2).
do_installer(PackageType, Descs) :-
	SourceDir = ~atom_concat(~component_src(ciaode), '/'),
	TargetDir = ~distpkg_output_dir,
	mkdir_perm(TargetDir, ~perms),
	string_to_file("", ~atom_concat(TargetDir, '/NODISTRIBUTE')),
	distpkg_name_version(PackageNameVersion),
	exclude_files(PackageType, ExcludeFiles),
	bold_message("Creating ~w tarball ~w ~w, please be patient ...",
	    [PackageType, PackageNameVersion, Descs]),
	atom_concat(TargetDir, '/', TargetDir2), % TODO: remove '/'
	build_distpkg_codeitem_contents(PackageType, SourceDir, TargetDir2,
	    PackageNameVersion, ExcludeFiles, FileList),
	list(Descs, build_distpkg_codeitem(SourceDir, TargetDir2,
		PackageNameVersion, PackageType, FileList)).

% TODO: Used from the compile farm (SHARED file)
descfile <- :- descfile.

:- export(descfile/0).
descfile :-
	distpkg_name(PackageName),
	distpkg_name_version(PackageNameVersion),
	distpkg_version(PackageVersion),
	distpkg_version_nice(PackageVersionNice),
	svn_repository(SvnRepository),
	distpkg_revision(PackageRevision),
	svn_revision_time(SvnRepository, PackageRevision, PackageSvnTime),
	%
	TargetDir = ~distpkg_output_dir,
	mkdir_perm(TargetDir, ~perms),
	DescFile = ~atom_concat(TargetDir, '/desc.tmpl'),
	output_to_file(distpkg_generate_meta(PackageName,
		PackageNameVersion, PackageVersion, PackageVersionNice,
		PackageSvnTime, distpkg_codeitem_kind, distpkg_docitem_kind),
	    DescFile).

% ===========================================================================

:- use_module(library(format)).

:- use_module(library(make(system_extra))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).

% TODO: Used from the compile farm (SHARED file)
generate_revision <- [] :- generate_revision.

% (Re)generate REVISION file if current repository version has changed.
:- export(generate_revision/0).
generate_revision :-
	ciaode_revision_string(Revision),
	revision_file(RevisionFile),
	( file_exists(RevisionFile),
	    file_to_string(RevisionFile, RevisionOld) ->
	    ( Revision = RevisionOld ->
		format("NOTE: Revision file is up to date (~s)\n",
		    [Revision])
	    ; format("NOTE: Updating revision file (~s->~s)\n",
		    [RevisionOld, Revision]),
	      write_revision(Revision, RevisionFile)
	    )
	; format("NOTE: Creating revision file (~s)\n", [Revision]),
	  write_revision(Revision, RevisionFile)
	).

write_revision(Revision, VersionFile) :-
	string_to_file(Revision, VersionFile).









