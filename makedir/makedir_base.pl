:- module(_, _, [make, fsyntax]).
:- doc(title,  "CiaoDE Compilation/Installation module").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(autoconfig)).
:- use_module(library(format)).
:- use_module(library(file_utils)).
:- use_module(library(distutils)).
:- use_module(library(aggregates)).

:- use_module(library(make(system_extra))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).

test_tty <- :-
	( using_tty ->
	    display('\\r\n')
	; display('\\n\n')
	).

generate_revision <- [] # "(Re)generate REVISION file if current " ||
	"repository version has changed." :-
	generate_revision.

generate_revision :-
	ciaode_revision_string(Revision),
	revision_file(RevisionFile),
	( file_exists(RevisionFile),
	    file_to_string(RevisionFile, RevisionOld) ->
	    ( Revision = RevisionOld ->
		format("NOTE: Revision file is up to date (~s)\n",
		    [Revision])
	    ; format("NOTE: Updating revision file (~s->~s)\n",
		    [RevisionOld, Revision]),
		write_revision(Revision, RevisionFile)
	    )
	; format("NOTE: Creating revision file (~s)\n", [Revision]),
	    write_revision(Revision, RevisionFile)
	).

write_revision(Revision, VersionFile) :-
	string_to_file(Revision, VersionFile).
