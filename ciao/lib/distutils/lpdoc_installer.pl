:- module(lpdoc_installer, [], [assertions, regtypes, fsyntax]).

% TODO: THIS MUST BE PART OF LPDOC!

:- use_module(library(make(system_extra))).

:- export(install_info_dir/2).
install_info_dir(FinalFile, InstallDir) :-
	atom_concat(InstallDir, '/dir', DirFile),
	( file_exists(DirFile) -> true
	% TODO: 'infodir' must live in the lpdoc source (lpdoc/lib), not here!
	; copy_file(~absolute_file_name(library(distutils(infodir))),
		DirFile, [append])
	),
	do(['install-info --dir-file=', DirFile, ' ', FinalFile],
	    '/dev/null', '/dev/null', []).

:- export(uninstall_info_dir/2).
uninstall_info_dir(FinalFile, InstallDir) :-
	atom_concat(InstallDir, '/dir', DirFile),
	do(['install-info --remove --dir-file=', DirFile, ' ',
		FinalFile], '/dev/null', '/dev/null', []).

