% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "CiaoDE Compilation/Installation module").
:- doc(author, "Edison Mera").
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
:- use_module(library(distutils)).
:- use_module(library(autoconfig)).
:- use_module(library(distutils(readme_generator))).
:- use_module(library(distutils(distclean))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
:- use_module(ciaodesrc(makedir('ConfigMenu'))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(library(aggregates)).
:- use_module(library(unittest)).

% Define options used to build the Win32 installer
:- use_module(ciaodesrc(makedir(makedir_win32))).

% Define options used to build the rpm package
:- use_module(ciaodesrc(makedir(makedir_rpm))).

% Define options used to build the mac package and macport protfile
:- use_module(ciaodesrc(makedir(makedir_mac))).

% Define options used to build the source/binary installer
:- use_module(ciaodesrc(makedir(makedir_src))).
:- use_module(ciaodesrc(makedir(makedir_bin))).
:- use_module(ciaodesrc(makedir(makedir_base))).

:- use_module(ciaodesrc(makedir('DOCCOMMON')), [docdir/1]).

:- doc(module, "

@section{Main CiaoDE Makefile}

bootclean <- realclean + Remove the static lpmake and engine.  Note
	that if you do it, you must execute the ./configure script
	again.").


all <- [allciao, chr, allextra] :- true.

'ciaosetup_modules/ciaosetup.opts' <- 'ciao/makedir/MenuOptions.pl'
	:: File :-
	output_to_file(ciaosetup_opts, File).

allciao <- :-
	lpmake_subdir(ciao, makedir_part_ciao, all).

chr <- :-
	lpmake_subdir(ciao, makedir_part_ciao, chr).

allextra <- :-
	make_extracomponent(all).

platdep <- [] :-
	make_component(platdep).

make_extracomponent(Target) :-
	(
	    extracomponent(P),
	    ( lpmake_subdir(P, ~atom_concat(makedir_part_, P), Target)
	    -> true ),
	    fail
	;
	    true
	).

make_component(Target) :-
	(
	    component(P),
	    ( lpmake_subdir(P, ~atom_concat(makedir_part_, P), Target) -> true ),
	    fail
	;
	    true
	).

allnolibsextra <- :-
	make_extracomponent(allnolibs).

librariesextra <- :-
	make_extracomponent(libraries).

applicationsextra <- :-
	make_extracomponent(applications).

docs <- [docsreadmes] :-
	make_component(docs).

'build/doc' <- :-
	mkdir_perm('build', ~perms),
	mkdir_perm('build/doc', ~perms).

docsreadmes <- ['build/doc'] :-
	output_to_file(ciaosetup_opts, 'doc/readmes/configure_help.tmp'),
	SrcDir = ~atom_concat([~component_src(ciaode), '/doc/readmes']),
	DocDir = 'build/doc',
	Files = [as('README_CIAODE', 'README'),
	         as('DEVEL_CIAODE', 'DEVEL')],
	generate_readme_files(Files, SrcDir, DocDir).

install <- :-
	install.

installdoc <- :-
	installdoc.
installdoc :-
	make_component(installdoc).

install :-
	install_ciao,
	install_extras.

install_ciao :-
	lpmake_subdir(ciao, makedir_part_ciao, install).

install_extras <- :-
	install_extras.

install_extras :-
	make_extracomponent(install).

uninstall <- :-
	uninstall.

uninstall :-
	uninstall_extras,
	uninstall_ciao.

uninstall_extras :-
	make_extracomponent(uninstall).

uninstall_ciao :-
	bold_message("Uninstalling Ciao"),
	lpmake_subdir(ciao, makedir_part_ciao, uninstall),
	make_subdir(~lpmake, ~gmake, 'ciao', '', uninstalleng,
	    ~command_option),
	bold_message("Ciao uninstallation completed").

unreconfigure <- :-
	unreconfigure.

unreconfigure :-
	make_extracomponent(unreconfigure),
	lpmake_subdir(ciao, makedir_part_ciao, unreconfigure).

reconfigure <- :-
	lpmake_subdir(ciao, makedir_part_ciao, reconfigure),
	make_extracomponent(reconfigure),
	bold_message(
"Your initialization files have been modified for Ciao execution.
You must make sure they are re-read (by, e.g., logging out and
back into your system) before using any Ciao component.").

bootclean <- :-
	do(['rm -rf bin'], fail).

runtests <- [] # "Run CiaoDE tests" :-
	runtestsciao,
	runtestsextra.

runtestsciao :-
	lpmake_subdir(ciao, makedir_part_ciao, runtests).

runtestsextra :-
	make_extracomponent(runtests).

runbenchmarks <- [] # "Run CiaoDE benchmarks" :-
	runbenchmarksciao,
	runbenchmarksextra.

runbenchmarksciao :-
	lpmake_subdir(ciao, makedir_part_ciao, runbenchmarks).

runbenchmarksextra :-
	make_extracomponent(runbenchmarks).
