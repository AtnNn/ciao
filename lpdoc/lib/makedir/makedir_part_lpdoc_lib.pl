% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "Lpdoc library Compilation/Installation module").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(terms)).
:- use_module(library(distutils)).
:- use_module(library(system)).
:- use_module(library(make(system_extra))).

:- use_module(ciaodesrc(makedir('DOCCOMMON')), [perms/1, docdir/1]).
:- use_module(ciaodesrc(makedir('ConfigValues')), [instype/1,
		get_lpdoclibdir/1, lpdoclibbasedir/2, instype/1]).

:- use_module(lpdocsrc(makedir('LPDOCSETTINGS'))).

:- use_module(lpdocsrc(makedir('LPDOCSHARED'))).

libfiles := '*.el|*.elc|*.tex|*.bst|*.info|*.html|*.css|SETTINGS_DEFAULT.pl|texi2html'.

'DOTcshrc' := ~atom_concat([~build_root, ~get_lpdoclibdir, '/',
		'DOTcshrc']).
~'DOTcshrc' <- :-
	replace_strings_in_file([
		["binary_directory",        ~atom_codes(~bindir)],
		["documentation_directory", ~atom_codes(~docdir)]],
	    'DOTcshrc.skel', ~'DOTcshrc'),
	-set_perms(~'DOTcshrc', ~perms).

'DOTprofile' := ~atom_concat([~build_root, ~get_lpdoclibdir, '/',
		'DOTprofile']).
~'DOTprofile' <- :-
	replace_strings_in_file([
		["binary_directory",        ~atom_codes(~bindir)],
		["documentation_directory", ~atom_codes(~docdir)]],
	    'DOTprofile.skel', ~'DOTprofile'),
	-set_perms(~'DOTprofile', ~perms).

'word-help-setup.el' := ~atom_concat([~build_root, ~get_lpdoclibdir,
		'/word-help-setup.el']).
~'word-help-setup.el' <- :-
	build_root(BuildRoot),
	get_lpdoclibdir(LibDir),
	atom_concat([BuildRoot, LibDir, '/'], RLibDir),
	replace_strings_in_file([["library_directory", ~atom_codes(LibDir)
		]],
	    'word-help-setup.el', ~'word-help-setup.el'),
	do(['cd ', RLibDir, '; emacs -batch -f batch-byte-compile `ls *.el`'
	    ],
	    nofail),
	-set_exec_perms(~add_preffix(~ls(LibDir, '*.el|*.elc'), RLibDir),
	    ~perms).

rbasemain := ~atom_concat([~build_root, ~baselibdir, '/', ~basemain]).
~rbasemain <- [] :: RBaseMain :-
	--copy_file(~versionmain, RBaseMain, [overwrite, symlink]).

install <- :-
	justinstall.

justinstall :- justinstall_(~instype).
justinstall_(src).
justinstall_(ins) :- doinstall.

doinstall :-
	get_lpdoclibdir(LibDir),
	build_root(RpmBuildRoot),
	atom_concat(RpmBuildRoot, LibDir, BuildLibDir),
	atom_concat(BuildLibDir,  '/',    RBuildLibDir),
	mkdir_perm(BuildLibDir, ~perms),
	ls(~libfiles, LibFiles),
	copy_files(LibFiles, BuildLibDir, [overwrite]),
	-set_perms(~add_preffix(LibFiles, RBuildLibDir), ~perms),
	-set_exec_perms(~atom_concat(RBuildLibDir, 'texi2html'), ~perms),
	make([~'DOTcshrc', ~'DOTprofile', ~rbasemain,
		~'word-help-setup.el']).
% 	make(~rversionmain),

uninstall <- :- justuninstall.

justuninstall :- justuninstall_(~instype).

justuninstall_(src).
justuninstall_(ins) :- douninstall.

douninstall :-
	delete_dir_rec(~atom_concat(~build_root,
		~lpdoclibbasedir(~instype))).

% 	get_lpdoclibdir(LibDir),
% 	atom_concat(LibDir, '/', RLibDir),
% 	del_files_nofail(~add_preffix(~ls(LibDir, ~libfiles), RLibDir)),
% 	del_files_nofail(~add_preffix(['texi2html', 'DOTcshrc',
% 	'DOTprofile'], RLibDir)),
% 	-(del_file_nofail(~rbasemain)),
% 	(
% 	    file_exists(LibDir) ->
% 	    -(delete_directory(LibDir))
% 	;
% 	    true
% 	).

clean <- :-
	del_files_nofail(~ls('*~|*.TMP|*.po|*.itf')).

distclean <- [clean] :- true.
