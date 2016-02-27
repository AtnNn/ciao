:- module(_, _, [make, fsyntax]).

:- doc(title,  "Source Installer Generation").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(ciaodesrc(makedir(distpkg_gen_common))).
:- use_module(ciaodesrc(makedir(makedir_component))).
:- use_module(library(distutils(distpkg_generator))).

installer_src <- [] :- installer_src.
installer_src :-
	generate_revision,
	do_installer(src, [tgz, tbz]).

installer_tgz <- [] :- installer_tgz.
installer_tgz :-
	generate_revision,
	do_installer(src, [tgz]).

installer_tbz <- [] :- installer_tbz.
installer_tbz :-
	generate_revision,
	do_installer(src, [tbz]).

packagefilename(tgz) := ~atom_concat([
		'package/', ~component_packname_version_patch_rev(~component_wholesystem), '.tar.gz']).

packagefilename(tbz) := ~atom_concat([
		'package/', ~component_packname_version_patch_rev(~component_wholesystem), '.tar.bz2']).

% The next options will work only if the package not exist yet:
~packagefilename(tgz) <- [] :- installer_tgz.
~packagefilename(tbz) <- [] :- installer_tbz.
