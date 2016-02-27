:- module(git_tools, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

% TODO: what about contrib/subversion/subversion.pl?

:- doc(title, "Git tools"). 
:- doc(author, "Jose F. Morales").

:- doc(summary, "This module defines predicates to interact with a Git
   repository").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system)).
:- use_module(library(system_extra), [do_str_without_nl/3]).

% ---------------------------------------------------------------------------

:- export(git_repo_at_dir/1).
% There is a Git repository at Dir
git_repo_at_dir(Dir) :-
	file_exists(~atom_concat(Dir, '/.git')).

% ---------------------------------------------------------------------------

:- export(git_output/3).
% Execute a Git command on the specified directory, ignore standard
% error, and get standard output as an atom.
git_output(Path, Command, R) :-
	do_str_without_nl(['(cd ', Path, ' && git ', Command, ' 2>/dev/null)'], fail, R0),
	atom_codes(R, R0).


