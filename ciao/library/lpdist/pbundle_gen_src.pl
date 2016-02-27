:- module(pbundle_gen_src, _, [make, fsyntax]).
% TODO: Export list?

:- doc(title,  "pbundle Generation as Source Tar").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(pbundle_gen_common))).
:- use_module(library(lpdist(pbundle_generator))).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

gen_pbundle__src <- [] :- gen_pbundle__src.
gen_pbundle__src :-
	gen_bundle_commit_info,
	gen_pbundle__common(src, [tgz, tbz]).

gen_pbundle__tgz <- [] :- gen_pbundle__tgz.
gen_pbundle__tgz :-
	gen_bundle_commit_info,
	gen_pbundle__common(src, [tgz]).

gen_pbundle__tbz <- [] :- gen_pbundle__tbz.
gen_pbundle__tbz :-
	gen_bundle_commit_info,
	gen_pbundle__common(src, [tbz]).

% TODO: used from pbundle_gen_mac too
pbundle_packname_absfile(tgz) := F :-
	Bundle = ~bundle_wholesystem,
	F = ~fsR(concat_k(ext('.tar.gz'), build/pbundle/(~bundle_versioned_packname(Bundle)))).

pbundle_packname_absfile(tbz) := F :-
	Bundle = ~bundle_wholesystem,
	F = ~fsR(concat_k(ext('.tar.bz2'), build/pbundle/(~bundle_versioned_packname(Bundle)))).

% The next options will work only if the package not exist yet:
~pbundle_packname_absfile(tgz) <- [] :- gen_pbundle__tgz.
~pbundle_packname_absfile(tbz) <- [] :- gen_pbundle__tbz.
