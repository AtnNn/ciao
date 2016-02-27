:- module(pbundle_gen_bin, _, [make, fsyntax]).

% TODO: What is a binary installer?
:- doc(title,  "pbundle Generation as Binary Installers").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(lpdist(pbundle_gen_common))).
:- use_module(library(lpdist(pbundle_generator))).
:- use_module(library(lpdist(ciao_bundle_db))).

% TODO: Offer a simpler interface (with just gen_pbundle and
%       arguments, reflecting the new options for ciaosetup)

gen_pbundle__bin <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(bin, [tgz, tbz]).
gen_pbundle__bin_tgz <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(bin, [tgz]).
gen_pbundle__bin_tbz <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(bin, [tbz]).

gen_pbundle__noa <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(noa, [tgz, tbz]).
gen_pbundle__noa_tgz <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(noa, [tgz]).
gen_pbundle__noa_tbz <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(noa, [tbz]).

gen_pbundle__raw <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(raw, [tgz, tbz]).
gen_pbundle__raw_tgz <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(raw, [tgz]).
gen_pbundle__raw_tbz <- [] :-
	gen_bundle_commit_info,
	gen_pbundle__common(raw, [tbz]).
