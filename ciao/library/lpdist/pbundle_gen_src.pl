:- module(pbundle_gen_src, _, [make, fsyntax]).
% TODO: Export list?

:- doc(title,  "pbundle Generation as (Universal) Source Package").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(pbundle_gen_common))).
:- use_module(library(lpdist(pbundle_generator))).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

gen_pbundle__src <- [] :- gen_pbundle__src.
gen_pbundle__src :-
	gen_bundle_revision,
	gen_pbundle__common(src, [tgz, tbz]).

gen_pbundle__tgz <- [] :- gen_pbundle__tgz.
gen_pbundle__tgz :-
	gen_bundle_revision,
	gen_pbundle__common(src, [tgz]).

gen_pbundle__tbz <- [] :- gen_pbundle__tbz.
gen_pbundle__tbz :-
	gen_bundle_revision,
	gen_pbundle__common(src, [tbz]).

% TODO: used from pbundle_gen_mac too
pbundle_name(tgz) :=
 	~fsR(concat_k(ext('.tar.gz'), build/pbundle/(~bundle_packname_version_patch_rev(~bundle_wholesystem)))).

pbundle_name(tbz) :=
 	~fsR(concat_k(ext('.tar.bz2'), build/pbundle/(~bundle_packname_version_patch_rev(~bundle_wholesystem)))).

% The next options will work only if the package not exist yet:
~pbundle_name(tgz) <- [] :- gen_pbundle__tgz.
~pbundle_name(tbz) <- [] :- gen_pbundle__tbz.
