% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "CiaoDE Compilation/Installation module"              ).
:- doc(author, "Edison Mera"                                         ).
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(ciaodesrc(makedir(makedir_common))).
:- use_module(ciaodesrc(makedir(makedir_base))).
:- use_module(library(distutils(distpkg_generator))).

installer_bin <- [generate_revision] :- do_installer(bin, [tgz, tbz]).
installer_bin_tgz <- [generate_revision] :- do_installer(bin, [tgz]).
installer_bin_tbz <- [generate_revision] :- do_installer(bin, [tbz]).

installer_noa <- [generate_revision] :- do_installer(noa, [tgz, tbz]).
installer_noa_tgz <- [generate_revision] :- do_installer(noa, [tgz]).
installer_noa_tbz <- [generate_revision] :- do_installer(noa, [tbz]).

installer_raw <- [generate_revision] :- do_installer(raw, [tgz, tbz]).
installer_raw_tgz <- [generate_revision] :- do_installer(raw, [tgz]).
installer_raw_tbz <- [generate_revision] :- do_installer(raw, [tbz]).
