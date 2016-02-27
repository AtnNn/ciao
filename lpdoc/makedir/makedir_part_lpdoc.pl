% ===========================================================================
:- module(_, _, [ciaopaths, make, fsyntax]).
% ===========================================================================
:- doc(title,  "Lpdoc Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(terms)).
:- use_module(library(system)).

% ============================================================================

:- include(library(lpdist('makedir_SHARE'))).
bundle_id(lpdoc).
bundle_dname('LPdoc').
bundle_readme_dir := ~fsR(bundle_src(lpdoc)/'readmes').
bundle_manual_dir := ~fsR(bundle_src(lpdoc)/'doc').
%
bundle_readme(as('INSTALLATION_LPDOC', 'INSTALLATION')).
bundle_readme(as('README_LPDOC', 'README')).
%
bundle_ins_reg. % TODO: document (register the bundle on ins?)

% ============================================================================
% COMPILATION
% ============================================================================

% (hook)
bundle_build <- [] :-
	prebuild_libraries,
	bundle_bin.

% (invoked from 'makedir', for each bundle)
build_nolibs <- [] :-
	prebuild_libraries,
	bundle_bin.

% (invoked from 'makedir', for each bundle)
build_platdep <- [] :-
	bundle_bin.

% (invoked from 'makedir', for each 'extra' bundle)
build_applications <- [] :- true.

% (invoked from 'makedir', for each bundle)
build_libraries <- [] :- true.

% Prepare the code
prebuild_libraries <- [] :- prebuild_libraries.
prebuild_libraries :-
	make(['src/version_auto.pl']).

%% ---------------------------------------------------------------------------

:- use_module(library(format), [format/3]).

'src/version_auto.pl' <- [] :: File # "Generation of version_auto.pl file" :-
	open(File, write, O),
	format(O, "%% Do not edit - automatically generated!\n", []),
	format(O, "version('~w.~w: ~s (compiled with ~w ~w.~w)').\n",
	    [~bundle_version(lpdoc), ~bundle_patch(lpdoc), ~datime_string,
	     ~bundle_packname(ciao), ~bundle_version(ciao), ~bundle_patch(ciao)]),
	close(O),
	-set_perms(File, ~perms).

% TODO: use build_standalone_list/3
bundle_bin :-
 	b_make_exec(lpdoc, bundle_src(lpdoc)/src, 'lpdoc', same_process).

% ============================================================================
:- doc(section, "Documentation").
% ============================================================================

docs <- [] # "Creates all the documentation files." :-
	docs_readmes,
	docs_manuals.

% =============================================================================
% REGISTER
% =============================================================================

bundle_register_hook.
bundle_unregister_hook.

% =============================================================================
% INSTALLATION                                                              
% =============================================================================

bundle_install <- [] :-
	bundle_install(~instype).

bundle_install(local) :-
	bold_message("Skipping copy of LPdoc files."),
	bundle_install_lib,
	install_docs.
bundle_install(global) :-
	bold_message("Installing LPdoc."),
	bundle_install_lib,
	install_bin,
	install_docs,
	bold_message("LPdoc installation completed").

bundle_uninstall <- :-
	bundle_uninstall(~instype).

bundle_uninstall(local) :-
	bold_message("Skipping deletion of LPdoc files."),
	bundle_uninstall_lib.
bundle_uninstall(global) :-
	bold_message("Uninstalling LPdoc"),
	uninstall_docs,
	bundle_uninstall_lib,
	uninstall_bin,
	bold_message("LPdoc uninstallation completed").

install_docs <- :- install_docs.
install_docs :-
	bundle_install_docs(lpdoc).

uninstall_docs :-
	bundle_uninstall_docs(lpdoc).

install_bin <- ['src/version_auto.pl'] # "Installation of LPdoc executable."
	:-
	install_bin.
install_bin :-
	b_install_copy_and_link(plexe, lpdoc, 'lpdoc').

uninstall_bin :-
	% TODO: seems to be a problem with links.... (why? JFMC)
	b_uninstall_copy_and_link(plexe, lpdoc, 'lpdoc').

% ===========================================================================

:- use_module(library(unittest), [run_test_dir/2]).

runtests <- [] :-
	bold_message("Running LPdoc tests."),
	run_test_dir(~fsR(bundle_src(lpdoc)), [rtc_entry]).

runbenchmarks <- [] :- true.

% ===========================================================================

:- use_module(library(lpdist(ciao_config_options)), [perms/1, docdir/1]).

bundle_install_lib :- install_lib_(~instype).

install_lib_(local).
install_lib_(global) :-
	b_install_dir(~fsR(bundle_inslib(lpdoc))),
	% TODO: verify that it is fine (previously it copied just files)
	b_install_dir_rec(bundle_src(lpdoc)/lib, ~fsR(bundle_inslib(lpdoc))),
	gen_DOTcshrc,
	gen_DOTprofile,
	gen_lpdoc_lib_link.
%	make([~'word-help-setup.el']).

% (only ins)
gen_DOTcshrc :-
	wr_template(inslib(lpdoc), bundle_src(lpdoc)/lib, 'DOTcshrc', [
	    'binary_directory' = ~ciaobin_dir,
	    'documentation_directory' = ~docdir
        ]).

% (only ins)
gen_DOTprofile :-
	wr_template(inslib(lpdoc), bundle_src(lpdoc)/lib, 'DOTprofile', [
	    'binary_directory' = ~ciaobin_dir,
	    'documentation_directory' = ~docdir
	]).

% TODO: call directly? (see install_lib/0)
gen_lpdoc_lib_link :-
	% TODO: when is this link used? is this link repeated?
	--copy_file(~fsR(concat_verk(lpdoc, plexe, 'lpdoc')),
	            ~fsR(rootprefix(concat_k(plexe, ~ciaolib_root/'lpdoc'))),
		    [overwrite, symlink]).

bundle_uninstall_lib :- uninstall_lib_(~instype).
uninstall_lib_(local).
uninstall_lib_(global) :-
	b_uninstall_dir_rec(~fsR(bundle_insbaselib(lpdoc))).

% ===========================================================================
% TODO: Outdated code, move somewhere else?

% 	LibDir = ~fsR(bundle_inslib(lpdoc)),
% 	atom_concat(LibDir, '/', RLibDir),
% 	del_files_nofail(~add_prefix(~ls(LibDir, ~libfiles), RLibDir)),
% 	del_files_nofail(~add_prefix(['DOTcshrc',
% 	'DOTprofile'], RLibDir)),
% 	-(del_file_nofail(~rbasemain_dir)),
% 	(
% 	    file_exists(LibDir) ->
% 	    -(delete_directory(LibDir))
% 	;
% 	    true
% 	).

% < % (only ins)
% < gen_word_help_setup_el :-
% < 	% TODO: Is this part of the emacs mode or the other way around?
% < 	wr_template(inslib(lpdoc), bundle_src(lpdoc)/lib, 'word-help-setup.el',
% <             ['library_directory' = ~lpdoclib_dir]),
% < 	%
% < 	RLibDir = ~fsR(bundle_inslib(lpdoc)),
% < 	% TODO: find from prolog and execute as a loop
% < 	emacs_batch_call(~fsR(rootprefix(RLibDir)),
% < 	                 'emacs_lpdoc', % TODO: right log name?
% < 	                 '-f batch-byte-compile `ls *.el`'),
% < 	% TODO: why exec perms on .el and .elc?
% < 	-set_exec_perms(~add_prefix(~ls(~fsR(rootprefix(RLibDir)), '*.el|*.elc'),
% < 	                ~atom_concat(RLibDir, '/')),
% < 			~perms).

% 'word-help-setup.el' := ~atom_concat([~rootprefix, ~fsR(bundle_inslib(lpdoc)), '/word-help-setup.el']).
% ~'word-help-setup.el' <- :-
% 	rootprefix(BuildRoot),
% 	LibDir = ~fsR(bundle_inslib(lpdoc)),
% 	atom_concat([BuildRoot, LibDir, '/'], RLibDir),
% 	replace_strings_in_file(
%            [["library_directory", ~atom_codes(LibDir)]],
% 	   'lib/word-help-setup.el.skel', ~'word-help-setup.el'),
% 	do(['cd ', RLibDir, '; emacs -batch -f batch-byte-compile `ls *.el`'], nofail),
% 	-set_exec_perms(~add_prefix(~ls(LibDir, '*.el|*.elc'), RLibDir), ~perms).
