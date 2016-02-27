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
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(engine(system_info), [get_ciao_ext/1, get_exec_ext/1]).

:- use_module(library(make(system_extra))).
:- use_module(library(distutils), [component_make/2, lpmake_subdir/3]).
:- use_module(library(component_registry)). % TODO: Refine
:- use_module(ciaodesrc(makedir(makedir_aux))).
:- use_module(ciaodesrc(makedir(makedir_component))).
:- use_module(ciaodesrc(makedir(makedir_distclean))).
:- use_module(ciaodesrc(makedir('ConfigMenu'))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(library(aggregates)).
:- use_module(library(unittest)).

% ---------------------------------------------------------------------------
% Targets for the distpkg generation

:- use_module(ciaodesrc(makedir(distpkg_gen_win32))).
:- use_module(ciaodesrc(makedir(distpkg_gen_rpm))).
:- use_module(ciaodesrc(makedir(distpkg_gen_mac))).
:- use_module(ciaodesrc(makedir(distpkg_gen_src))).
:- use_module(ciaodesrc(makedir(distpkg_gen_bin))).
:- use_module(ciaodesrc(makedir(distpkg_gen_common))).

% ---------------------------------------------------------------------------

:- use_module(ciaodesrc(makedir('DOCCOMMON')), [docdir/1]).

% bootclean <- realclean + Remove the static lpmake and engine.  Note
% 	that if you do it, you must execute the ./configure script
% 	again.

% ============================================================================

:- include(ciaodesrc(makedir('makedir_SHARE'))).
component_id(ciaode).
component_dname('CiaoDE'). % the whole system...
component_readme_dir('doc/readmes').
component_readme(as('README_CIAODE', 'README')).
component_readme(as('DEVEL_CIAODE', 'DEVEL')).
component_manual_dir(_) :- fail.

% ============================================================================

component_all <- [all_ciao, all_chr, all_extra] :- true.

'makedir/ciaosetup_modules/ciaosetup.opts' <- 'ciao/makedir/MenuOptions.pl'
	:: File :-
	output_to_file(ciaosetup_opts, File).

all_ciao <- :- component_make(ciao, all).

all_chr <- :- component_make(ciao, chr). % TODO: CHR should be a subcomponent of libs or contrib

all_extra <- :-
	extra_components_make(all).

platdep <- [] :-
	all_components_make(platdep).

% invoke lpmake Target on all extra components
extra_components_make(Target) :-
	( % (failure-driven loop)
          extra_component(P),
	    component_make(P, Target),
	    fail
	; true
	).

% invoke lpmake Target on all components
all_components_make(Target) :-
	( % (failure-driven loop)
	  component(P),
	    component_make(P, Target),
	    fail
	; true
	).

% invoke lpmake Target on all components (reverse order)
allrev_components_make(Target) :-
	( % (failure-driven loop)
	  ( extra_component(P)
	  ; basic_component(P)
	  ),
	    component_make(P, Target),
	    fail
	; true
	).

allnolibsextra <- :-
	extra_components_make(allnolibs).

librariesextra <- :-
	extra_components_make(libraries).

applicationsextra <- :-
	extra_components_make(applications).

docs <- ['build/doc', docs_readmes] :-
	all_components_make(docs).

'build/doc' <- :-
	mkdir_perm('build', ~perms),
	mkdir_perm('build/doc', ~perms).

component_register <- :- true.
component_unregister <- :- true.

component_install <- :-
	component_install.
component_install :-
	all_components_make(install).

install_extras <- :-
	install_extras.

install_extras :-
	extra_components_make(install).

component_uninstall <- :-
	component_uninstall.

component_uninstall :-
	% (uninstall must be performed in reverse dependency order)
	allrev_components_make(uninstall).

% ---------------------------------------------------------------------------

% TODO: automatically done by install
installdoc <- :-
	installdoc.
installdoc :-
	all_components_make(installdoc).

% ---------------------------------------------------------------------------

% TODO: automatically done by install
register_all <- :-
	all_components_make(component_register),
	register_message.

% TODO: Is this message shown in normal installations?
% ((un)register_all is automatically invoked from (un)install)
register_message :-
	bold_message(
"Your initialization files have been modified for Ciao execution.\n"||
"You must make sure they are re-read (by, e.g., logging out and\n"||
"back into your system) before using any Ciao component.").

% TODO: automatically done by uninstall
unregister_all <- :-
	unregister_all.

unregister_all :-
	% (unregister must be performed in reverse dependency order)
	allrev_components_make(component_unregister).

% ---------------------------------------------------------------------------

:- use_module(library(component_registry), [show_components/0]).

% (available from the command line)
show_components <- :-
	show_components.

% ---------------------------------------------------------------------------

bootclean <- :-
	do(['rm -rf bin'], fail).

runtests <- [] # "Run CiaoDE tests" :-
	all_components_make(runtests).

runbenchmarks <- [] # "Run CiaoDE benchmarks" :-
	all_components_make(runbenchmarks).
