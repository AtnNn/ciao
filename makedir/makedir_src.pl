% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "CiaoDE Compilation/Installation module"              ).
:- doc(author, "Edison Mera"                                         ).
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================
:- use_module(library(terms), [atom_concat/2]).
:- use_module(ciaodesrc(makedir(makedir_common))).
:- use_module(ciaodesrc(makedir(makedir_base))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
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
		'package/', ~version(~wholesystem), '.tar.gz']).

packagefilename(tbz) := ~atom_concat([
		'package/', ~version(~wholesystem), '.tar.bz2']).

% The next options will work only if the package not exist yet:
~packagefilename(tgz) <- [] :- installer_tgz.
~packagefilename(tbz) <- [] :- installer_tbz.
