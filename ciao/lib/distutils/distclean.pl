:- module(_, _, [make, fsyntax, regexp]).

:- use_module(library(system)).
:- use_module(library(make(system_extra))).
:- use_module(library(distutils(distclean_common))).

comment(module, "

@section{cleaning routines}

Have in mind the next definitions:

distclean <- rawclean + remove compiled files, including texic files,
	     but preserve documentation.

realclean <- distclean + remove all auto generated files, including
             documentation.

").

% Specific files that must be deleted in order to clean CiaoDE

clean_specific := 'ciao/SETTINGS'|'ciao/SETTINGS_AUTO'.

distclean <- :-
	display('Cleaning readmes...\n'),
	recursive_readmes_distclean,
	display('Recursive distclean...\n'),
	recursive_distclean('./'),
	(
	    --delete_file(~clean_specific),
	    fail
	;
	    true
	).

realclean <- :-
	display('Cleaning readmes...\n'),
	recursive_readmes_realclean,
	display('Cleaning lpdoc...\n'),
	recursive_lpdoc_docsclean,
	display('Recursive realclean...\n'),
	recursive_realclean('.').
