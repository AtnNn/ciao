:- module(info_installer, [], [assertions, regtypes, fsyntax]).

:- doc(title, "Installation of info files").
:- doc(author, "The CLIP group").

:- doc(module, "This module defines predicates to install @tt{info}
   files").

% TODO: It seems that both this module and library/lpdist/infodir
%       should be part of LPdoc.

:- use_module(library(system_extra)).

:- export(dirfile_install_info/2).
:- pred dirfile_install_info(InstallDir, InfoFile) 
   # "Install @var{InfoFile} in the @tt{dir} file at
     @var{InstallDir}. If no @tt{dir} file exist, a default one is
     created.".

dirfile_install_info(InstallDir, InfoFile) :-
	DirFile = ~info_dirfile(InstallDir),
	% Create a info 'dir' file, if it does not exist
	ensure_dirfile(DirFile),
	do(['install-info',
	    ' --dir-file=', DirFile,
	    ' ', InfoFile],
	   '/dev/null', '/dev/null', []).

:- export(dirfile_uninstall_info/2).
:- pred dirfile_uninstall_info(InstallDir, InfoFile) 
   # "Uninstall @var{InfoFile} from the @tt{dir} file at 
     @var{InstallDir}.".

dirfile_uninstall_info(InstallDir, InfoFile) :-
	DirFile = ~info_dirfile(InstallDir),
	do(['install-info',
	    ' --remove',
	    ' --dir-file=', DirFile,
	    ' ', InfoFile],
	   '/dev/null', '/dev/null', []).

info_dirfile(Path) := ~atom_concat(Path, '/dir').

ensure_dirfile(DirFile) :-
	( file_exists(DirFile) ->
	    true
	; % TODO: 'infodir' should live in the lpdoc source (lpdoc/lib)
	  copy_file(~absolute_file_name(library(lpdist(infodir))),
	    DirFile, [append])
	).
