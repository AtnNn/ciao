:- module(listfiles, [current_filter_files/9, is_distributable_dir/6],
	[regexp]).

:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(distutils(find))).

is_distributable_dir(CurrBaseDir, BaseDir, SkipDirs, SkipSubDirs, NDFiles,
	    Dir) :-
	member(NDFile, NDFiles),
	atom_concat([CurrBaseDir, Dir, '/', NDFile], NDFileName),
	file_exists(NDFileName) ->
	fail
    ;
	atom_concat(BaseDir,   RelSubDir, CurrBaseDir),
	atom_concat(RelSubDir, Dir,       SubDir),
	\+(match_pred(SkipDirs,    Dir)),
	\+(match_pred(SkipSubDirs, SubDir)).

current_filter_files(BaseDir, Pattern, SkipFiles, SkipDirs, SkipSubDirs,
	    NDFiles, Dir, File, FileName) :-
	current_find(BaseDir, is_distributable_dir(BaseDir, SkipDirs,
		SkipSubDirs, NDFiles), dir_after_dummy_, file, Dir, File,
	    FileName),
	list_filter_file_(Dir, File, Pattern, SkipFiles).

list_filter_file_(Dir, File, Pattern, SkipFiles) :-
	atom_concat(Dir, File, FileName),
	\+(file_property(FileName, linkto(_))),
	\+(match_pred(SkipFiles,   File)),
	match_pred(Pattern, File),
	!.
	
