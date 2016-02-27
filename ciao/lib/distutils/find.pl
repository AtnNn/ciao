:- module(find, [], [hiord]).

:- use_module(library(system)).

:- export(path_concat/3).
path_concat(Dir, '.', Dir).
path_concat(A,   B,   R  ) :-
	atom_concat(_, '/', A),
	!,
	atom_concat(A, B, R).
path_concat(A, B, R) :-
	atom_concat(A,  '/', A0),
	atom_concat(A0, B,   R ).

:- export(path_list_concat/2).
% todo: this is not tail-recursive, fix
path_list_concat([], '') :- !.
path_list_concat([X], X) :- !.
path_list_concat([X|Xs], R) :-
	path_list_concat(Xs, B),
	path_concat(X, B, R).

:- export(find/4).
:- meta_predicate find(?, pred(2), pred(2), pred(2)).

find(BaseDir, FileCondition, DirBefore, DirAfter) :-
	( current_find(BaseDir, DirBefore, DirAfter, file, CurrentDir,
	    CurrentFile, _CurrentFileName),
	  FileCondition(CurrentDir, CurrentFile),
	  fail
        ; true
	).

:- export(dir_before_dummy_/2).
dir_before_dummy_(_, _).
:- export(dir_after_dummy_/2).
dir_after_dummy_(_, _).

:- export(dir_before_display_/2).
dir_before_display_(BaseDir, File) :-
	display_list(['Entering ', BaseDir, File, '\n']).

:- export(dir_after_display_/2).
dir_after_display_(BaseDir, File) :-
	display_list(['Leaving  ', BaseDir, File, '\n']).

% todo: not currently used?
:- export(file_display_/2).
file_display_(BaseDir, File) :-
	display_list(['Process  ', BaseDir, File, '\n']).

:- export(current_find/7).
:- meta_predicate current_find(?, pred(2), pred(2), ?, ?, ?, ?).
current_find(BaseDir0, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName) :-
	path_concat(BaseDir0, '', BaseDir),
	current_find_(BaseDir, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName).

:- meta_predicate current_find_(?, pred(2), pred(2), ?, ?, ?, ?).
current_find_(BaseDir, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName) :-
	directory_files(BaseDir, Files),
	member(File, Files),
	\+ member(File, ['.', '..']),
	current_find_rec(BaseDir, File, DirBefore, DirAfter,
	    CurrentType, CurrentDir, CurrentFile, CurrentFileName).

:- export(current_find_rec/8).
% todo: missing meta_predicate (performance will be low)
current_find_rec(BaseDir, File, DirBefore, DirAfter, CurrentType,
	    CurrentDir, CurrentFile, CurrentFileName) :-
	atom_concat(BaseDir, File, FileName),
	(
	    is_dir(FileName) ->
	    (
		CurrentType = dir,
		CurrentDir = BaseDir,
		CurrentFile = File,
		CurrentFileName = FileName
	    ;
		atom_concat(FileName, '/', BaseSubDir),
		current_find_dir(BaseDir, File, BaseSubDir, DirBefore,
		    DirAfter, CurrentType, CurrentDir, CurrentFile,
		    CurrentFileName)
	    )
	;
	    CurrentType = file,
	    CurrentDir = BaseDir,
	    CurrentFile = File,
	    CurrentFileName = FileName
	).

:- export(current_find_dir/9).
% todo: missing meta_predicate (performance will be low)
current_find_dir(BaseDir, File, BaseSubDir, DirBefore, DirAfter,
	    CurrentType, CurrentDir, CurrentFile, CurrentFileName) :-
	DirBefore(BaseDir, File),
	(
	    current_find_(BaseSubDir, DirBefore, DirAfter, CurrentType,
	        CurrentDir, CurrentFile, CurrentFileName)
	;
	    DirAfter(BaseDir, File),
	    fail
	).

:- export(is_dir/1).
is_dir(FileName) :-
	\+(file_property(FileName, linkto(_))),
	file_exists(FileName),
	file_property(FileName, type(directory)).

% find('/home/edison/mnt/workspace/JavaTest/',file_display_, dir_before_display_, dir_after_display_).
