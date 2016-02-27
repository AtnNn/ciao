% ===========================================================================
:- module(_, _, [ciaopaths, make, fsyntax]).
% ===========================================================================
:- doc(title,  "Lpdoc Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(compiler(exemaker)), [make_exec/2]).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(format), [format/3]).
:- use_module(library(distutils), [lpmake_subdir/3]).
:- use_module(library(distutils(dirutils))).
:- use_module(library(make(system_extra))).
:- use_module(library(make(make_rt))).
:- use_module(library(component_registry(component_registry_base))).
:- use_module(ciaodesrc(makedir('ConfigValues')), [instype/1, build_doc_dir/1,
		set_configured_flags/0, srcbindir/1, get_lpdoclibdir/1,
		reallibdir/1, ciaobinroot/1]).
:- use_module(ciaodesrc(makedir('ConfigValues')), [
		lpdoclibbasedir/2, ciaolibroot/1, ciaobinroot/1, build_root/1]).
:- use_module(ciaodesrc(makedir(makedir_component))).
:- use_module(ciaodesrc(makedir(makedir_aux))).
:- use_module(library(component_registry), [component_src/2]).
:- use_module(library(unittest)).

dirs := [src, lib, doc, examples].

basemain := 'lpdoc'.

installedvpmain := ~atom_concat([~build_root, ~ciaobinroot, '/', ~component_name_version(lpdoc),
		~get_ciao_ext]).

installedmain := ~atom_concat([~build_root, ~ciaobinroot, '/', ~basemain]).

% ============================================================================

:- include(ciaodesrc(makedir('makedir_SHARE'))).
component_id(lpdoc).
component_dname('LPdoc').
component_readme_dir('readmes').
component_readme(as('INSTALLATION_LPDOC', 'INSTALLATION')).
component_readme(as('README_LPDOC', 'README')).
component_manual_dir('doc').

% ============================================================================
% COMPILATION
% ============================================================================

default <- [all] :- true.

% (private)
component_all <- [extra_libraries, createbin] :- true.

platdep <- [compiling_message,
	    createbin,
	    compilation_done_message] :- true.

allnolibs <- all :- true.

applications <- all :- true.

libraries <- [] :- true.

% Libraries that require customized installation
% (such as automatically generated code, foreign interfaces, etc.)
extra_libraries <- ['src/version_auto.pl'] :- true.

%% ---------------------------------------------------------------------------
'src/version_auto.pl' <- [~atom_concat(~component_src(ciao), '/SETTINGS')]
	:: File # "Generation of version_auto.pl file" :-
	open(File, write, O),
	format(O, "%% Do not edit - automatically generated!\n", []),
	format(O, "version('~w.~w of ~s (compiled with ~w)').\n",
	    [~component_version(lpdoc), ~component_patch(lpdoc), ~datime_string, ~component_name_version_patch(ciao)]),
	close(O),
	-set_perms(File, ~perms).

createbin <- [] # "Generation of lpdoc executable." :-
	dolpdoc.

dolpdoc :-
	atom_concat([~component_src(lpdoc), '/src/', ~basemain, '.pl'],
	    AbsSourceFile),
	set_configured_flags,
	make_exec([AbsSourceFile], ~srcbinlpdoc),
	--copy_file(~atom_concat(~component_name_version(lpdoc), ~get_ciao_ext),
	    ~atom_concat([~srcbindir, '/', ~basemain]), [overwrite,
		symlink]),
	-set_perms(~atom_concat([~srcbindir, '/', ~basemain]), ~perms).

%% Command used for compiling lpdoc
:- use_module(ciaodesrc(makedir('ConfigValues')), [srcbindir/1]).
srcbinlpdoc := ~atom_concat([~srcbindir, '/', ~basemain, '-', ~component_version(lpdoc),
		~get_ciao_ext]).

% ============================================================================
% CREATE DOCUMENTATION
% ============================================================================

docs <- [] # "Creates all the documentation files." :-
	docs_readmes,
	docs_manuals.

:- include(lpdocsrc(makedir('CONFIG'))).

component_register <- :-
	component_register(~instype).
component_register(src).
component_register(ins) :-
	install_alias_paths('lpdoc_ins_auto.pl', ~build_root, ~reallibdir,
	    ~abs_alias_paths(~ins_alias_paths, ~get_lpdoclibdir)).

component_unregister <- :-
	component_unregister(~instype).
component_unregister(src).
component_unregister(ins) :-
	uninstall_alias_paths('lpdoc_ins_auto.pl', ~build_root, ~reallibdir).

% =============================================================================
% INSTALLATION                                                              
% =============================================================================

component_install <- [] :-
	component_install(~instype).

component_install(src) :-
	bold_message("Skipping copy of LPdoc files."),
	installdoc.
component_install(ins) :-
	bold_message("Installing lpdoc."),
	justinstalllib,
	installbin,
	installdoc,
	bold_message("LPdoc installation completed").

component_uninstall <- :-
	component_uninstall(~instype).

component_uninstall(src) :-
	bold_message("Skipping deletion of LPdoc files.").
component_uninstall(ins) :-
	bold_message("Uninstalling LPdoc"),
	uninstalldoc,
	justuninstalllib,
	uninstallbin,
	bold_message("LPdoc uninstallation completed").

installdoc <- :- installdoc.
installdoc :-
	component_install_docs(lpdoc).

uninstalldoc :-
	component_uninstall_docs(lpdoc).

installbin <- ['src/version_auto.pl'] # "Installation of lpdoc executable."
	:-
	installbin.
installbin :-
	srcbindir(SrcBinDir),
	ciaobinroot(BinDir),
	perms(ExecMode),
	atom_concat(~build_root, BinDir, BuildBinDir),
	mkdir_perm(BuildBinDir, ExecMode),

	copy_file(~atom_concat([SrcBinDir, '/', ~component_name_version(lpdoc),
		    ~get_ciao_ext]), BuildBinDir, [overwrite]),

	copy_file(~atom_concat([~component_name_version(lpdoc), ~get_ciao_ext]),
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

% ===========================================================================
% ===========================================================================
% ===========================================================================

:- use_module(ciaodesrc(makedir('DOCCOMMON')), [perms/1, docdir/1]).

libfiles := '*.el|*.elc|*.tex|*.bst|*.info|*.html|*.css|SETTINGS_DEFAULT.pl'.

'DOTcshrc' := ~atom_concat([~build_root, ~get_lpdoclibdir, '/', 'DOTcshrc']).
~'DOTcshrc' <- :-
	replace_strings_in_file([
		["binary_directory",        ~atom_codes(~ciaobinroot)],
		["documentation_directory", ~atom_codes(~docdir)]],
	    'lib/DOTcshrc.skel', ~'DOTcshrc'),
	-set_perms(~'DOTcshrc', ~perms).

'DOTprofile' := ~atom_concat([~build_root, ~get_lpdoclibdir, '/', 'DOTprofile']).
~'DOTprofile' <- :-
	replace_strings_in_file([
		["binary_directory",        ~atom_codes(~ciaobinroot)],
		["documentation_directory", ~atom_codes(~docdir)]],
	    'lib/DOTprofile.skel', ~'DOTprofile'),
	-set_perms(~'DOTprofile', ~perms).

'word-help-setup.el' := ~atom_concat([~build_root, ~get_lpdoclibdir,
		'/word-help-setup.el']).
~'word-help-setup.el' <- :-
	build_root(BuildRoot),
	get_lpdoclibdir(LibDir),
	atom_concat([BuildRoot, LibDir, '/'], RLibDir),
	replace_strings_in_file([["library_directory", ~atom_codes(LibDir)
		]],
	    'lib/word-help-setup.el', ~'word-help-setup.el'),
	do(['cd ', RLibDir, '; emacs -batch -f batch-byte-compile `ls *.el`'
	    ],
	    nofail),
	-set_exec_perms(~add_preffix(~ls(LibDir, '*.el|*.elc'), RLibDir),
	    ~perms).

% TODO: Why?
rbasemain := ~atom_concat([~build_root, ~ciaolibroot, '/', ~basemain]).
~rbasemain <- [] :: RBaseMain :-
	--copy_file(~component_name_version(lpdoc), RBaseMain, [overwrite, symlink]).

justinstalllib :- justinstalllib_(~instype).
justinstalllib_(src).
justinstalllib_(ins) :- doinstalllib.

doinstalllib :-
	get_lpdoclibdir(LibDir),
	build_root(RpmBuildRoot),
	atom_concat(RpmBuildRoot, LibDir, BuildLibDir),
	atom_concat(BuildLibDir,  '/',    RBuildLibDir),
	mkdir_perm(BuildLibDir, ~perms),
	ls(~libfiles, LibFiles),
	copy_files(LibFiles, BuildLibDir, [overwrite]),
	-set_perms(~add_preffix(LibFiles, RBuildLibDir), ~perms),
	make([~'DOTcshrc', ~'DOTprofile', ~rbasemain,
		~'word-help-setup.el']).

justuninstalllib :- justuninstalllib_(~instype).

justuninstalllib_(src).
justuninstalllib_(ins) :- douninstalllib.

douninstalllib :-
	delete_dir_rec(~atom_concat(~build_root,
		~lpdoclibbasedir(~instype))).

% 	get_lpdoclibdir(LibDir),
% 	atom_concat(LibDir, '/', RLibDir),
% 	del_files_nofail(~add_preffix(~ls(LibDir, ~libfiles), RLibDir)),
% 	del_files_nofail(~add_preffix(['DOTcshrc',
% 	'DOTprofile'], RLibDir)),
% 	-(del_file_nofail(~rbasemain)),
% 	(
% 	    file_exists(LibDir) ->
% 	    -(delete_directory(LibDir))
% 	;
% 	    true
% 	).

