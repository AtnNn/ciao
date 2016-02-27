% ===========================================================================
:- module(_, _, [ciaopaths, make, fsyntax]).
% ===========================================================================
:- doc(title,  "CiaoDE Compilation/Installation module").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(messages)).
:- use_module(library(file_utils)).

%% :- use_module(library(system_extra)).
:- use_module(library(bundle_registry(bundle_registry_load))). % TODO: Refine
:- use_module(library(lpdist(distutils)), [bundle_invoke_lpmake/2, lpmake_subdir/3]).
%% :- use_module(library(lpdist(makedir_aux))).
%% :- use_module(library(lpdist(ciao_bundle_db))).
%% :- use_module(library(lpdist(ciao_configure))).
%% :- use_module(library(lpdist(ciao_config_options))).
%% :- use_module(library(aggregates)).
:- use_module(library(unittest)).

% TODO: Disabled, see the source.
%% :- use_module(library(lpdist(makedir_distclean))).
% TODO: Do not use lpmake here... just a command line tool

% ---------------------------------------------------------------------------
% Targets for the pbundle generation

:- use_module(library(lpdist(pbundle_gen_win32))).
:- use_module(library(lpdist(pbundle_gen_rpm))).
:- use_module(library(lpdist(pbundle_gen_mac))).
:- use_module(library(lpdist(pbundle_gen_src))).
:- use_module(library(lpdist(pbundle_gen_bin))).
:- use_module(library(lpdist(pbundle_gen_common))).

% ============================================================================

:- include(library(lpdist('makedir_SHARE'))).
bundle_id(ciaode).
bundle_dname('CiaoDE'). % the whole system...
%
bundle_readme_dir := ~fsR(bundle_src(ciaode)/'doc'/'readmes').
bundle_manual_dir := _ :- fail.
%
bundle_readme(as('README_CIAODE', 'README')).
bundle_readme(as('DEVEL_CIAODE', 'DEVEL')).
%
bundle_ins_reg :- fail. % TODO: document (register the bundle on ins?)

% ============================================================================

:- use_module(library(lpdist(ciao_bundle_db)), [gen_bundle_revision/0]).
% TODO: Used from the Ciao Bot (SHARED file)
gen_bundle_revision <- [] :- gen_bundle_revision.

% ============================================================================

bundle_build <- [build_ciao, build_chr, build_extra] :- true.

% TODO: Invoking this by hand everytime that configuration options
%       have changed is really unpleasant...
'makedir/config_opts.txt' <- 'ciao/library/lpdist/ciao_config_options.pl' :: File :-
	output_to_file(show_config_opts, File).

build_ciao <- :- bundle_invoke_lpmake(ciao, build).

build_chr <- :- bundle_invoke_lpmake(ciao, build_chr). % TODO: CHR should be a sub-bundle of libs or contrib

build_extra <- :-
	extra_bundles_make(build).

build_platdep <- [] :-
	all_bundles_make(build_platdep).

% invoke lpmake Target on all extra bundles
extra_bundles_make(Target) :-
	( % (failure-driven loop)
          extra_bundle(P),
	    bundle_invoke_lpmake(P, Target),
	    fail
	; true
	).

% invoke lpmake Target on all registered bundles
all_bundles_make(Target) :-
	( % (failure-driven loop)
	  registered_bundle(P),
	    bundle_invoke_lpmake(P, Target),
	    fail
	; true
	).

% invoke lpmake Target on all registered bundles, in reverse order
allrev_bundles_make(Target) :-
	( % (failure-driven loop)
	  ( extra_bundle(P)
	  ; basic_bundle(P)
	  ),
	    bundle_invoke_lpmake(P, Target),
	    fail
	; true
	).

build_nolibs_extra <- :-
	extra_bundles_make(build_nolibs).

build_extra_libraries <- :-
	extra_bundles_make(build_libraries).

docs <- [] :-
 	prepare_doc_dirs,
	docs_readmes, % TODO: share code
	all_bundles_make(docs).

bundle_register_hook.
bundle_unregister_hook.

bundle_install <- :-
	bundle_install.
bundle_install :-
	all_bundles_make(install).

install_extras <- :-
	install_extras.

install_extras :-
	extra_bundles_make(install).

bundle_uninstall <- :-
	bundle_uninstall.

bundle_uninstall :-
	% (uninstall must be performed in reverse dependency order)
	allrev_bundles_make(uninstall).

% ---------------------------------------------------------------------------

% TODO: automatically done by install
install_docs <- :-
	install_docs.
install_docs :-
	all_bundles_make(install_docs).

% ---------------------------------------------------------------------------

% TODO: automatically done by install
register_all <- :-
	all_bundles_make(bundle_register),
	register_message.

% TODO: Is this message shown in normal installations?
% ((un)register_all is automatically invoked from (un)install)
register_message :-
	bold_message(
"Your initialization files have been modified for Ciao execution.\n"||
"You must make sure they are re-read (by, e.g., logging out and\n"||
"back into your system) before using any Ciao bundle.").

% TODO: automatically done by uninstall
unregister_all <- :-
	unregister_all.

unregister_all :-
	% (unregister must be performed in reverse dependency order)
	allrev_bundles_make(bundle_unregister).

% ---------------------------------------------------------------------------

:- use_module(library(bundle_registry(bundle_registry_load)), [show_bundles/0]).

% (available from the command line)
show_bundles <- :-
	show_bundles.

% ---------------------------------------------------------------------------

runtests <- [] # "Run CiaoDE tests" :-
	all_bundles_make(runtests).

runbenchmarks <- [] # "Run CiaoDE benchmarks" :-
	all_bundles_make(runbenchmarks).
