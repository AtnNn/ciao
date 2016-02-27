% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "Lpdoc Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(compiler(exemaker)), [make_exec/2]).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(format), [format/3]).
:- use_module(library(distutils), [build_root/1, lpmake_subdir/3]).
:- use_module(library(distutils(readme_generator))).
:- use_module(library(make(system_extra))).
:- use_module(library(make(make_rt))).
:- use_module(library(autoconfig(autoconfig_base))).
:- use_module(lpdocsrc(makedir('LPDOCSETTINGS'))).
:- use_module(lpdocsrc(makedir('LPDOCSHARED'))).
:- use_module(ciaodesrc(makedir('ConfigValues')), [instype/1,
		set_configured_flags/0, srcbindir/1, get_lpdoclibdir/1,
		reallibdir/1]).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
:- use_module(library(autoconfig)).
:- use_module(library(unittest)).

dirs := [src, lib, doc, examples].

installedvpmain := ~atom_concat([~build_root, ~bindir, '/', ~versionmain,
		~get_ciao_ext]).

installedmain := ~atom_concat([~build_root, ~bindir, '/', ~basemain]).


% ============================================================================
% COMPILATION
% ============================================================================

default <- [all] :- true.

all <- [compiling_message,
	    environment,
	    automodules,
	    createbin,
	    compilation_done_message] :- true.

platdep <- [compiling_message,
	    environment,
	    createbin,
	    compilation_done_message] :- true.

allnolibs <- all :- true.

applications <- all :- true.

libraries <- [] :- true.

compiling_message <- :-
	bold_message("Compiling Lpdoc").

compilation_done_message <- :-
	bold_message("Lpdoc compilation completed").

automodules <- ['src/version_auto.pl']
# "Build Automatically generated modules" :- true.

%% ---------------------------------------------------------------------------
'src/version_auto.pl' <- [~atom_concat(~component_src(ciao), '/SETTINGS')]
	:: File # "Generation of version_auto.pl file" :-
	open(File, write, O),
	format(O, "%% Do not edit - automatically generated!\n", []),
	format(O, "version('~w.~w of ~s (~w)').\n",
	    [~vers, ~patch, ~datime_string, ~engine]),
	close(O),
	-set_perms(File, ~perms).

mainext := ~atom_concat(['src/', ~mainname, '.pl']).

% createbin <- ['src/version_auto.pl', 'src/lpdoc.pl',
% 	    'src/autodoc.pl', ~mainext]
% 	# "Generation of (dynamic) lpdoc executable." :-
% 	dolpdoc(~pl2dynexe, fail).

% TODO: This underestimates the ciaoc ability to track dependencies, removed --JF
% createbin <- ['src/autodoc.pl',
% 	    'src/autodoc_ascii.pl',
% 	    'src/autodoc_html.pl',
% 	    'src/autodoc_newhtml.pl',
% 	    'src/autodoc_man.pl',
% 	    'src/autodoc_texinfo.pl',
% 	    'src/autodocformats.pl',
% 	    'src/comments.pl',
% 	    'src/lpdoc.pl',
% 	    'src/version_auto.pl',
% 	    'src/',
% 	    ~mainext] # "Forced generation of (static) lpdoc executable." :-
% 	dolpdoc.
createbin <- [] # "Forced generation of (static) lpdoc executable." :-
	dolpdoc.

dolpdoc :-
	atom_concat([~component_src(lpdoc), '/src/', ~basemain, '.pl'],
	    AbsSourceFile),
	set_configured_flags,
	make_exec([AbsSourceFile], ~srcbinlpdoc),
	--copy_file(~atom_concat(~versionmain, ~get_ciao_ext),
	    ~atom_concat([~srcbindir, '/', ~basemain]), [overwrite,
		symlink]),
	-set_perms(~atom_concat([~srcbindir, '/', ~basemain]), ~perms).

environment <- [] :- true.

% ============================================================================
% CREATE DOCUMENTATION
% ============================================================================

docs <- [environment, docsreference, docsreadmes]
# "Creates all the documentation files." :- true.

docsreference <- [] # "Creates the reference documentation." :-
	docsreference(all).

docsreference(Options) :-
	invoke_lpdoc(~atom_concat(~component_src(lpdoc), '/doc/SETTINGS'),
	    Options).

docsreadmes <- [] # "Creates the README files." :-
	SrcDir = ~atom_concat([~component_src(lpdoc), '/readmes']),
	DocDir = ~build_doc_dir,
	Files = [as('INSTALLATION_LPDOC', 'INSTALLATION'),
		 as('README_LPDOC', 'README')],
	generate_readme_files(Files, SrcDir, DocDir).

:- include(lpdocsrc(makedir('CONFIG'))).

reconfigure <- :-
	reconfigure(~instype).
reconfigure(src).
reconfigure(ins) :-
	install_alias_paths('lpdoc_ins_auto.pl', ~build_root, ~reallibdir,
	    ~abs_alias_paths(~ins_alias_paths, ~get_lpdoclibdir)).

unreconfigure <- :-
	unreconfigure(~instype).
unreconfigure(src).
unreconfigure(ins) :-
	uninstall_alias_paths('lpdoc_ins_auto.pl', ~build_root, ~reallibdir).

% =============================================================================
% INSTALLATION                                                              
% =============================================================================

install <- [justinstall, reconfigure] # "LPdoc Installation" :- true.
uninstall <- [justuninstall, unreconfigure] # "LPdoc Uninstallation" :- true.

justinstall <- [] :-
	justinstall(~instype).

justinstall(src) :-
	bold_message("Skipping copy of LPdoc files."),
	installdoc.
justinstall(ins) :-
	bold_message("Installing lpdoc."),
	installlib,
	installbin,
	installdoc,
	bold_message("LPdoc installation completed").

justuninstall <- :-
	justuninstall(~instype).

justuninstall(src) :-
	bold_message("Skipping deletion of LPdoc files.").
justuninstall(ins) :-
	bold_message("Uninstalling LPdoc"),
	uninstalldoc,
	uninstalllib,
	uninstallbin,
	bold_message("LPdoc uninstallation completed").

installdoc <- :- installdoc.
installdoc :-
	install_docdir(lpdoc).

uninstalldoc :-
	uninstall_docdir(lpdoc).

installlib :-
	lpmake_subdir(lib, makedir_part_lpdoc_lib, install).

uninstalllib :-
	lpmake_subdir(lib, makedir_part_lpdoc_lib, uninstall).

installbin <- ['src/version_auto.pl'] # "Installation of lpdoc executable."
	:-
	installbin.
installbin :-
	srcbindir(SrcBinDir),
	bindir(BinDir),
	perms(ExecMode),
	atom_concat(~build_root, BinDir, BuildBinDir),
	mkdir_perm(BuildBinDir, ExecMode),

	copy_file(~atom_concat([SrcBinDir, '/', ~versionmain,
		    ~get_ciao_ext]), BuildBinDir, [overwrite]),

	copy_file(~atom_concat([~versionmain, ~get_ciao_ext]),
	    ~atom_concat([BuildBinDir, '/', ~basemain]),
	    [symlink, overwrite]),
%	-- set_exec_perms( ~installedmain, ExecMode ),
	-- set_exec_perms(~installedvpmain, ExecMode).

uninstallbin :-
%% seems to be a problem with links....
	del_file_nofail(~installedmain),
	del_file_nofail(~installedvpmain).

runtests <- [] :-
	bold_message("Running LPdoc tests."),
	run_test_dir(~component_src(lpdoc), []).

runbenchmarks <- [] :- true.
